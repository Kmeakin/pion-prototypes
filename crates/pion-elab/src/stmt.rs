use pion_core::prim::Prim;
use pion_core::semantics::{Telescope, Type, Value};
use pion_core::{Expr, FunArg, FunParam, LetBinding};
use pion_diagnostic::{Diagnostic, DiagnosticHandler, Label};
use pion_surface::{self as surface, Located, Rec};
use text_size::TextRange;

use super::Elaborator;

impl<'core, 'text, 'surface, H> Elaborator<'core, 'text, H>
where
    H: DiagnosticHandler,
{
    pub fn synth_block(&mut self, block: &surface::Block<'surface>) -> (Expr<'core>, Type<'core>) {
        return recur(self, block.stmts, block.result_expr);

        fn recur<'surface, 'core, H: DiagnosticHandler>(
            this: &mut Elaborator<'core, '_, H>,
            stmts: &[Located<surface::Stmt<'surface>>],
            expr: Option<&'surface Located<surface::Expr<'surface>>>,
        ) -> (Expr<'core>, Type<'core>) {
            let [stmt, stmts @ ..] = stmts else {
                return match expr {
                    None => (Expr::RecordLit(&[]), Type::RecordType(Telescope::empty())),
                    Some(expr) => this.synth_expr(expr),
                };
            };

            match stmt.data {
                pion_surface::Stmt::Let(Rec::Nonrec, binding) => {
                    this.elab_let(&binding, |this| recur(this, stmts, expr))
                }
                pion_surface::Stmt::Let(Rec::Rec, binding) => {
                    this.elab_letrec(&binding, |this| recur(this, stmts, expr))
                }
            }
        }
    }

    pub(super) fn check_block(
        &mut self,
        range: TextRange,
        block: &surface::Block<'surface>,
        expected: &Type<'core>,
    ) -> Expr<'core> {
        return match block.result_expr {
            Some(expr) => recur(self, block.stmts, expr, expected),
            None => {
                let (expr, r#type) = self.synth_block(block);
                self.convert_expr(range, expr, r#type, expected)
            }
        };

        fn recur<'surface, 'core, H: DiagnosticHandler>(
            this: &mut Elaborator<'core, '_, H>,
            stmts: &[Located<surface::Stmt<'surface>>],
            expr: &'surface Located<surface::Expr<'surface>>,
            expected: &Type<'core>,
        ) -> Expr<'core> {
            let [stmt, stmts @ ..] = stmts else {
                return this.check_expr(expr, expected);
            };

            match stmt.data {
                pion_surface::Stmt::Let(Rec::Nonrec, binding) => {
                    let (expr, ()) = this.elab_let(&binding, |this| {
                        let expr = recur(this, stmts, expr, expected);
                        (expr, ())
                    });
                    expr
                }
                pion_surface::Stmt::Let(Rec::Rec, binding) => {
                    let (expr, ()) = this.elab_letrec(&binding, |this| {
                        let expr = recur(this, stmts, expr, expected);
                        (expr, ())
                    });
                    expr
                }
            }
        }
    }

    fn elab_let<T>(
        &mut self,
        surface_binding: &'surface surface::LetBinding<'surface>,
        mut elab_body: impl FnMut(&mut Self) -> (Expr<'core>, T),
    ) -> (Expr<'core>, T) {
        let surface::LetBinding {
            pat: surface_pat,
            r#type: surface_type,
            rhs: surface_init,
        } = surface_binding;
        let (pat, r#type) = self.synth_ann_pat(surface_pat, *surface_type);
        let init_expr = self.check_expr(surface_init, &r#type);

        let bindings = self.destruct_pat(&pat, &init_expr, &r#type, false);

        let (body_expr, body_type) = {
            let local_len = self.env.locals.len();
            self.push_let_bindings(&bindings);
            let (body_expr, body_type) = elab_body(self);
            self.env.locals.truncate(local_len);
            (body_expr, body_type)
        };

        let expr = Expr::lets(self.bump, &bindings, body_expr);
        (expr, body_type)
    }

    fn elab_letrec<T>(
        &mut self,
        surface_binding: &'surface surface::LetBinding<'surface>,
        mut elab_body: impl FnMut(&mut Self) -> (Expr<'core>, T),
    ) -> (Expr<'core>, T) {
        let surface::LetBinding {
            pat: surface_pat,
            r#type: surface_type,
            rhs: surface_init,
        } = surface_binding;
        let (pat, mut r#type) = self.synth_ann_pat(surface_pat, *surface_type);
        let name = pat.name();

        let init_expr = {
            let var = self.env.locals.next_var();
            self.env.locals.push_let(name, r#type.clone(), var);
            let init_expr = self.check_expr(surface_init, &r#type);
            self.env.locals.pop();
            init_expr
        };

        let r#type_expr = self.quote_env().quote(&r#type);
        let init_expr = match init_expr {
            Expr::FunLit { .. } => {
                r#type = self.elim_env().update_metas(&r#type);
                let Value::FunType { param, body } = &r#type else {
                    unreachable!()
                };

                let (param, output_type) = self.quote_env().quote_fun(*param, body.clone());

                let fix = &Expr::Prim(Prim::fix);
                Expr::FunApp {
                    fun: self.bump.alloc(Expr::FunApp {
                        fun: self.bump.alloc(Expr::FunApp {
                            fun: fix,
                            arg: FunArg::implicit(param.r#type),
                        }),
                        arg: FunArg::implicit(output_type),
                    }),
                    arg: FunArg::explicit(self.bump.alloc(Expr::FunLit {
                        param: FunParam::explicit(name, self.bump.alloc(r#type_expr)),
                        body: self.bump.alloc(init_expr),
                    })),
                }
            }
            Expr::Error => Expr::Error,
            _ => {
                let diagnostic = Diagnostic::error()
                    .with_message("recursive bindings must be function literals")
                    .with_labels(vec![Label::primary(self.file_id, surface_pat.range)]);
                self.report_diagnostic(diagnostic);
                Expr::Error
            }
        };

        let (body_expr, body_type) = {
            let init_value = self.eval_env().eval(&init_expr);
            self.env.locals.push_let(name, r#type.clone(), init_value);
            let (body_expr, body_type) = elab_body(self);
            self.env.locals.pop();
            (body_expr, body_type)
        };

        let (r#type, init, body) = self.bump.alloc((r#type_expr, init_expr, body_expr));
        let binding = LetBinding::new(name, &*r#type, &*init);
        let core_expr = Expr::Let { binding, body };
        (core_expr, body_type)
    }
}
