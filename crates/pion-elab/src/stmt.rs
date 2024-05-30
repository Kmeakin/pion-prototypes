use pion_core::prim::Prim;
use pion_core::semantics::Value;
use pion_core::{Expr, FunArg, FunParam, LetBinding};
use pion_diagnostic::{Diagnostic, DiagnosticHandler, Label};
use pion_surface::{self as surface, Located};

use super::Elaborator;

impl<'core, 'text, 'surface, H> Elaborator<'core, 'text, H>
where
    H: DiagnosticHandler,
{
    pub(super) fn elab_let<T>(
        &mut self,
        surface_binding: &'surface surface::LetBinding<'surface>,
        surface_body: &'surface Located<surface::Expr<'surface>>,
        mut elab_body: impl FnMut(
            &mut Self,
            &'surface Located<surface::Expr<'surface>>,
        ) -> (Expr<'core>, T),
    ) -> (Expr<'core>, T) {
        let surface::LetBinding {
            pat: surface_pat,
            r#type: surface_type,
            init: surface_init,
        } = surface_binding;
        let (pat, r#type) = self.synth_ann_pat(surface_pat, *surface_type);
        let init_expr = self.check_expr(surface_init, &r#type);

        let bindings = self.destruct_pat(&pat, &init_expr, &r#type, false);

        let (body_expr, body_type) = {
            let local_len = self.local_env.len();
            self.push_let_bindings(&bindings);
            let (body_expr, body_type) = elab_body(self, surface_body);
            self.local_env.truncate(local_len);
            (body_expr, body_type)
        };

        let expr = Expr::lets(self.bump, &bindings, body_expr);
        (expr, body_type)
    }

    pub(super) fn elab_letrec<T>(
        &mut self,
        surface_binding: &'surface surface::LetBinding<'surface>,
        surface_body: &'surface Located<surface::Expr<'surface>>,
        mut elab_body: impl FnMut(
            &mut Self,
            &'surface Located<surface::Expr<'surface>>,
        ) -> (Expr<'core>, T),
    ) -> (Expr<'core>, T) {
        let surface::LetBinding {
            pat: surface_pat,
            r#type: surface_type,
            init: surface_init,
        } = surface_binding;
        let (pat, mut r#type) = self.synth_ann_pat(surface_pat, *surface_type);
        let name = pat.name();

        let init_expr = {
            let var = self.local_env.next_var();
            self.local_env.push_let(name, r#type.clone(), var);
            let init_expr = self.check_expr(surface_init, &r#type);
            self.local_env.pop();
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
            self.local_env.push_let(name, r#type.clone(), init_value);
            let (body_expr, body_type) = elab_body(self, surface_body);
            self.local_env.pop();
            (body_expr, body_type)
        };

        let (r#type, init, body) = self.bump.alloc((r#type_expr, init_expr, body_expr));
        let binding = LetBinding::new(name, r#type as &_, init as &_);
        let core_expr = Expr::Let { binding, body };
        (core_expr, body_type)
    }
}
