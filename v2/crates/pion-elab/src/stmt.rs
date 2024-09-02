use pion_core::env::{EnvLen, RelativeVar};
use pion_core::prim::Prim;
use pion_core::semantics::{Telescope, Type, Value};
use pion_core::syntax::{Expr, FunArg, FunParam, LetBinding};
use pion_printer::{docs, BumpDocAllocator, DocAllocator as _};
use pion_surface::syntax::{self as surface, Located, Rec};
use pion_util::location::Location;
use text_size::TextRange;

use super::Elaborator;
use crate::diagnostics;

impl<'core, 'text, 'surface> Elaborator<'core, 'text> {
    fn elab_command(&mut self, command: Located<surface::Command<'surface>>) {
        match command.data {
            surface::Command::Check(expr) => {
                let (expr, r#type) = self.synth_expr(&expr);
                let r#type = self.quote_env().quote(&r#type);

                let expr = self.zonk_env().zonk(&expr);
                let r#type = self.zonk_env().zonk(&r#type);

                let alloc = BumpDocAllocator::new(self.bump);
                let unelaborator = pion_core::unelab::Unelaborator::new(
                    alloc,
                    pion_core::unelab::Config::default(),
                );

                let doc = unelaborator.ann_expr(&mut self.env.locals.names, &expr, &r#type);
                let pretty = doc.pretty(80).to_string();
                self.command_output.push(pretty);
            }
            surface::Command::Eval(surface_expr) => {
                let (core_expr, _) = self.synth_expr(&surface_expr);
                let core_expr = self.eval_env().normalize(&core_expr);
                let core_expr = self.zonk_env().zonk(&core_expr);

                let alloc = BumpDocAllocator::new(self.bump);
                let surface_printer = pion_surface::printer::Printer::new(alloc, self.text);
                let core_unelaborator = pion_core::unelab::Unelaborator::new(
                    alloc,
                    pion_core::unelab::Config::default(),
                );

                let surface_expr_doc = surface_printer.expr(&surface_expr.data);
                let core_expr_doc = core_unelaborator.expr(&mut self.env.locals.names, &core_expr);
                let doc = docs![
                    &alloc,
                    surface_expr_doc,
                    alloc.line(),
                    "⇝",
                    alloc.line(),
                    core_expr_doc
                ]
                .group();
                self.command_output.push(doc.pretty(80).to_string());
            }
            surface::Command::Show(name) => {
                let Some(var) = self.env.locals.lookup(name.data) else {
                    let var_loc = Location::new(self.file_id, name.range);
                    diagnostics::unbound_local_var(self, name.data, var_loc);
                    return;
                };

                match self.env.locals.infos.get_relative(var).unwrap() {
                    crate::LocalInfo::Param => {
                        let r#type = self.env.locals.types.get_relative(var).unwrap();
                        let r#type = self.quote_env().quote(r#type);
                        let r#type = self.zonk_env().zonk(&r#type);

                        let alloc = BumpDocAllocator::new(self.bump);
                        let unelaborator = pion_core::unelab::Unelaborator::new(
                            alloc,
                            pion_core::unelab::Config::default(),
                        );

                        let doc = unelaborator.expr(&mut self.env.locals.names, &r#type);
                        let pretty = doc.pretty(80).to_string();
                        self.command_output
                            .push(format!("parameter {} : {pretty}", name.data));
                    }
                    crate::LocalInfo::Let => {
                        let expr = self.env.locals.exprs.get_relative(var).unwrap().unwrap();
                        let expr = expr.shift(self.bump, EnvLen::from(usize::from(var) + 1));
                        let expr = self.zonk_env().zonk(&expr);

                        let r#type = self.env.locals.types.get_relative(var).unwrap();
                        let r#type = self.quote_env().quote(r#type);
                        let r#type = self.zonk_env().zonk(&r#type);

                        let alloc = BumpDocAllocator::new(self.bump);
                        let unelaborator = pion_core::unelab::Unelaborator::new(
                            alloc,
                            pion_core::unelab::Config::default(),
                        );

                        let doc = unelaborator.let_stmt(
                            &mut self.env.locals.names,
                            Some(name.data),
                            &r#type,
                            &expr,
                        );
                        let pretty = doc.pretty(80).to_string();
                        self.command_output.push(pretty);
                    }
                }
            }
        }
    }

    pub fn synth_block(&mut self, block: &surface::Block<'surface>) -> (Expr<'core>, Type<'core>) {
        return recur(self, block.stmts, block.result_expr);

        fn recur<'surface, 'core>(
            this: &mut Elaborator<'core, '_>,
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
                surface::Stmt::Let(Rec::Nonrec, binding) => {
                    this.elab_let(&binding, |this| recur(this, stmts, expr))
                }
                surface::Stmt::Let(Rec::Rec, binding) => {
                    this.elab_letrec(&binding, |this| recur(this, stmts, expr))
                }
                surface::Stmt::Command(command) => {
                    this.elab_command(command);
                    recur(this, stmts, expr)
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

        fn recur<'surface, 'core>(
            this: &mut Elaborator<'core, '_>,
            stmts: &[Located<surface::Stmt<'surface>>],
            expr: &'surface Located<surface::Expr<'surface>>,
            expected: &Type<'core>,
        ) -> Expr<'core> {
            let [stmt, stmts @ ..] = stmts else {
                return this.check_expr(expr, expected);
            };

            match stmt.data {
                surface::Stmt::Let(Rec::Nonrec, binding) => {
                    let (expr, ()) = this.elab_let(&binding, |this| {
                        let expr = recur(this, stmts, expr, expected);
                        (expr, ())
                    });
                    expr
                }
                surface::Stmt::Let(Rec::Rec, binding) => {
                    let (expr, ()) = this.elab_letrec(&binding, |this| {
                        let expr = recur(this, stmts, expr, expected);
                        (expr, ())
                    });
                    expr
                }
                surface::Stmt::Command(command) => {
                    this.elab_command(command);
                    recur(this, stmts, expr, expected)
                }
            }
        }
    }

    // FIXME: check patterns for exhaustiveness
    fn elab_let<T>(
        &mut self,
        surface_binding: &'surface surface::LetBinding<'surface>,
        mut elab_body: impl FnMut(&mut Self) -> (Expr<'core>, T),
    ) -> (Expr<'core>, T) {
        let surface::LetBinding {
            pat: surface_pat,
            r#type: surface_type,
            rhs: surface_rhs,
        } = surface_binding;
        let (pat, r#type) = self.synth_ann_pat(surface_pat, *surface_type);
        let rhs_expr = self.check_expr(surface_rhs, &r#type);

        let bindings = self.destruct_pat(&pat, &rhs_expr, &r#type, false);

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

    // FIXME: check patterns for exhaustiveness
    fn elab_letrec<T>(
        &mut self,
        surface_binding: &'surface surface::LetBinding<'surface>,
        mut elab_body: impl FnMut(&mut Self) -> (Expr<'core>, T),
    ) -> (Expr<'core>, T) {
        let surface::LetBinding {
            pat: surface_pat,
            r#type: surface_type,
            rhs: surface_rhs,
        } = surface_binding;
        let (pat, mut r#type) = self.synth_ann_pat(surface_pat, *surface_type);
        let name = pat.name();

        let rhs_expr = {
            let expr = Expr::LocalVar(RelativeVar::default());
            let var = self.env.locals.next_var();
            self.env.locals.push_let(name, expr, r#type.clone(), var);
            let rhs_expr = self.check_expr(surface_rhs, &r#type);
            self.env.locals.pop();
            rhs_expr
        };

        let r#type_expr = self.quote_env().quote(&r#type);
        let rhs_expr = match rhs_expr {
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
                        body: self.bump.alloc(rhs_expr),
                    })),
                }
            }
            Expr::Error => Expr::Error,
            _ => {
                let loc = Location::new(self.file_id, surface_pat.range);
                diagnostics::recursive_let_not_function(self, loc);
                Expr::Error
            }
        };

        let (body_expr, body_type) = {
            let rhs_value = self.eval_env().eval(&rhs_expr);
            self.env
                .locals
                .push_let(name, rhs_expr, r#type.clone(), rhs_value);
            let (body_expr, body_type) = elab_body(self);
            self.env.locals.pop();
            (body_expr, body_type)
        };

        let (r#type, rhs, body) = self.bump.alloc((r#type_expr, rhs_expr, body_expr));
        let binding = LetBinding::new(name, &*r#type, &*rhs);
        let core_expr = Expr::Let { binding, body };
        (core_expr, body_type)
    }
}
