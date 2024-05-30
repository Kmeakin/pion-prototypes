use ecow::eco_vec;
use pion_core::env::RelativeVar;
use pion_core::prim::Prim;
use pion_core::semantics::{Closure, Elim, Head, Telescope, Type, Value};
use pion_core::{Expr, FunArg, FunParam, Lit, Plicity};
use pion_diagnostic::{Diagnostic, DiagnosticHandler, Label};
use pion_surface::{self as surface, Located};
use pion_symbol::Symbol;
use pion_util::slice_vec::SliceVec;
use text_size::TextRange;

use super::{Elaborator, MetaSource};

impl<'core, 'text, 'surface, H> Elaborator<'core, 'text, H>
where
    H: DiagnosticHandler,
{
    pub fn synth_lit(
        &mut self,
        surface_lit: &Located<surface::Lit>,
    ) -> (Result<Lit, ()>, Type<'core>) {
        let mut parse_int = |base| {
            let text = &self.text[surface_lit.range];
            match u32::from_str_radix(text, base) {
                Ok(int) => Ok(Lit::Int(int)),
                Err(error) => {
                    self.report_diagnostic(
                        Diagnostic::error()
                            .with_message(format!("Invalid integer literal: {error}"))
                            .with_labels(vec![Label::primary(self.file_id, surface_lit.range)]),
                    );
                    Err(())
                }
            }
        };
        let (lit, r#type) = match surface_lit.data {
            surface::Lit::Bool(b) => (Ok(Lit::Bool(b)), Type::BOOL),
            surface::Lit::DecInt => (parse_int(10), Type::INT),
            surface::Lit::BinInt => (parse_int(2), Type::INT),
            surface::Lit::HexInt => (parse_int(16), Type::INT),
        };
        (lit, r#type)
    }

    pub fn synth_expr(
        &mut self,
        surface_expr: &'surface Located<surface::Expr<'surface>>,
    ) -> (Expr<'core>, Type<'core>) {
        match surface_expr.data {
            surface::Expr::Error => (Expr::Error, Type::Error),
            surface::Expr::Lit(lit) => {
                let (lit, r#type) = self.synth_lit(&lit);
                let expr = match lit {
                    Ok(lit) => Expr::Lit(lit),
                    Err(()) => Expr::Error,
                };
                (expr, r#type)
            }
            surface::Expr::LocalVar(Located { data: name, .. }) => {
                if let Some(var) = self.local_env.lookup(name) {
                    let r#type = self.local_env.types.get_relative(var).unwrap().clone();
                    return (Expr::LocalVar(var), r#type);
                }

                if let Some(prim) = Prim::from_symbol(name) {
                    let r#type = prim.r#type();
                    return (Expr::Prim(prim), r#type);
                }

                self.report_diagnostic(
                    Diagnostic::error()
                        .with_message(format!("Unbound variable: {name}"))
                        .with_labels(vec![Label::primary(self.file_id, surface_expr.range)]),
                );
                (Expr::Error, Type::Error)
            }
            surface::Expr::Hole => {
                let range = surface_expr.range;
                let r#type = self.push_unsolved_type(MetaSource::HoleType { range });
                let expr = self.push_unsolved_expr(MetaSource::HoleExpr { range }, r#type.clone());
                (expr, r#type)
            }
            surface::Expr::Paren(expr) => self.synth_expr(expr),
            surface::Expr::Ann { expr, r#type } => {
                let r#type = self.check_expr_is_type(r#type);
                let r#type = self.eval_env().eval(&r#type);
                let expr = self.check_expr(expr, &r#type);
                (expr, r#type)
            }
            surface::Expr::Let {
                rec: surface::Rec::Nonrec,
                binding,
                body,
            } => self.elab_let(&binding, body, Elaborator::synth_expr),
            surface::Expr::Let {
                rec: surface::Rec::Rec,
                binding,
                body,
            } => self.elab_letrec(&binding, body, Elaborator::synth_expr),
            surface::Expr::If { cond, then, r#else } => {
                let cond = self.check_expr(cond, &Type::BOOL);
                let (then, then_type) = self.synth_expr(then);
                let r#else = self.check_expr(r#else, &then_type);
                let (cond, then, r#else) = self.bump.alloc((cond, then, r#else));
                (Expr::MatchBool { cond, then, r#else }, then_type)
            }
            surface::Expr::Match { scrut, cases } => {
                let range = surface_expr.range;
                let source = MetaSource::MatchResultType { range };
                let r#type = self.push_unsolved_type(source);
                let expr = self.check_match_expr(range, scrut, cases, &r#type);
                (expr, r#type)
            }
            surface::Expr::FunArrow { plicity, lhs, rhs } => {
                let param_type = self.check_expr_is_type(lhs);
                let body = {
                    let param_type_value = self.eval_env().eval(&param_type);
                    self.local_env.push_param(None, param_type_value);
                    let body = self.check_expr_is_type(rhs);
                    self.local_env.pop();
                    body
                };
                let (param_type, body) = self.bump.alloc((param_type, body));
                let core_expr = Expr::FunType {
                    param: FunParam::new(plicity.into(), None, param_type),
                    body,
                };
                (core_expr, Type::TYPE)
            }
            surface::Expr::FunType { params, body } => self.synth_fun_type(params, body),
            surface::Expr::FunLit { params, body } => self.synth_fun_lit(params, body),
            surface::Expr::FunApp { fun, arg } => {
                let (mut fun_expr, mut fun_type) = self.synth_expr(fun);
                if arg.data.plicity == Plicity::Explicit {
                    (fun_expr, fun_type) = self.insert_implicit_apps(arg.range, fun_expr, fun_type);
                }
                let fun_type = self.elim_env().update_metas(&fun_type);

                let (param, body) = match fun_type {
                    Value::FunType { param, body } if arg.data.plicity == param.plicity => {
                        (param, body)
                    }
                    Value::FunType { param, .. } => {
                        let fun_type = self.quote_env().quote(&fun_type);
                        let fun_type = self.pretty(&fun_type);
                        let diagnostic = Diagnostic::error()
                            .with_message(format!(
                                "Applied {} argument when {} argument was expected",
                                arg.data.plicity.description(),
                                param.plicity.description()
                            ))
                            .with_labels(vec![
                                Label::primary(self.file_id, arg.range).with_message(format!(
                                    "{} argument",
                                    arg.data.plicity.description()
                                )),
                                Label::secondary(self.file_id, fun.range)
                                    .with_message(format!("function has type {fun_type}")),
                            ]);
                        self.report_diagnostic(diagnostic);
                        return (Expr::Error, Type::Error);
                    }
                    Value::Error => return (Expr::Error, Type::Error),
                    _ => {
                        let fun_type = self.quote_env().quote(&fun_type);
                        let fun_type = self.pretty(&fun_type);
                        self.report_diagnostic(
                            Diagnostic::error()
                                .with_message(format!("Expected function, found `{fun_type}`"))
                                .with_labels(vec![Label::primary(self.file_id, fun.range)]),
                        );
                        return (Expr::Error, Type::Error);
                    }
                };

                let arg_expr = self.check_expr(arg.data.expr, param.r#type);
                let arg = self.eval_env().eval(&arg_expr);
                let output_type = self.elim_env().apply_closure(body, arg);

                let (fun, arg_expr) = self.bump.alloc((fun_expr, arg_expr));
                let arg = FunArg::new(param.plicity, arg_expr as &_);
                let core_expr = Expr::FunApp { fun, arg };
                (core_expr, output_type)
            }
            surface::Expr::ListLit(surface_exprs) => {
                let Some((first_surface_expr, rest_surface_exprs)) = surface_exprs.split_first()
                else {
                    let elem_type = self.push_unsolved_type(MetaSource::ListElemType {
                        range: surface_expr.range,
                    });
                    return (
                        Expr::ListLit(&[]),
                        Type::Neutral(
                            Head::Prim(Prim::List),
                            eco_vec![Elim::FunApp(FunArg::explicit(elem_type))],
                        ),
                    );
                };

                let mut exprs = SliceVec::new(self.bump, surface_exprs.len());
                let (expr, elem_type) = self.synth_expr(first_surface_expr);
                exprs.push(expr);

                for surface_expr in rest_surface_exprs {
                    let expr = self.check_expr(surface_expr, &elem_type);
                    exprs.push(expr);
                }

                (
                    Expr::ListLit(exprs.into()),
                    Type::Neutral(
                        Head::Prim(Prim::List),
                        eco_vec![Elim::FunApp(FunArg::explicit(elem_type))],
                    ),
                )
            }
            surface::Expr::TupleLit(surface_exprs) => {
                let mut expr_fields = SliceVec::new(self.bump, surface_exprs.len());
                let mut type_fields = SliceVec::new(self.bump, surface_exprs.len());

                for (index, surface_expr) in surface_exprs.iter().enumerate() {
                    let (expr, r#type) = self.synth_expr(surface_expr);
                    let name = Symbol::tuple_index(index);
                    expr_fields.push((name, expr));
                    type_fields.push((name, self.quote_env().quote_at(&r#type, index)));
                }

                let telescope = Telescope::new(self.local_env.values.clone(), type_fields.into());
                (
                    Expr::RecordLit(expr_fields.into()),
                    Type::RecordType(telescope),
                )
            }
            surface::Expr::RecordType(surface_fields) => {
                let mut type_fields = SliceVec::new(self.bump, surface_fields.len());
                let local_len = self.local_env.len();

                for surface_field in surface_fields {
                    let name = surface_field.data.name.data;
                    let r#type = self.check_expr_is_type(&surface_field.data.r#type);
                    let r#type_value = self.eval_env().eval(&r#type);
                    type_fields.push((name, r#type));
                    self.local_env.push_param(Some(name), r#type_value);
                }
                self.local_env.truncate(local_len);

                (Expr::RecordType(type_fields.into()), Type::TYPE)
            }
            surface::Expr::RecordLit(surface_fields) => {
                let mut expr_fields = SliceVec::new(self.bump, surface_fields.len());
                let mut type_fields = SliceVec::new(self.bump, surface_fields.len());

                for (index, surface_field) in surface_fields.iter().enumerate() {
                    let (expr, r#type) = self.synth_expr(&surface_field.data.expr);
                    let name = surface_field.data.name.data;
                    expr_fields.push((name, expr));
                    type_fields.push((name, self.quote_env().quote_at(&r#type, index)));
                }

                let telescope = Telescope::new(self.local_env.values.clone(), type_fields.into());
                (
                    Expr::RecordLit(expr_fields.into()),
                    Type::RecordType(telescope),
                )
            }
            surface::Expr::RecordProj { scrut, name } => {
                let (scrut_expr, scrut_type) = self.synth_expr(scrut);

                let scrut_type = self.elim_env().update_metas(&scrut_type);

                match scrut_type {
                    Value::RecordType(mut telescope) => {
                        let Some(_) = telescope.fields.iter().find(|(n, _)| *n == name.data) else {
                            self.report_diagnostic(
                                Diagnostic::error()
                                    .with_message(format!("Field `{}` not found", name.data))
                                    .with_labels(vec![Label::primary(self.file_id, name.range)]),
                            );
                            return (Expr::Error, Type::Error);
                        };

                        let scrut_value = self.eval_env().eval(&scrut_expr);
                        while let Some((n, r#type, update_telescope)) =
                            self.elim_env().split_telescope(&mut telescope)
                        {
                            if n == name.data {
                                let expr = Expr::RecordProj(self.bump.alloc(scrut_expr), name.data);
                                return (expr, r#type);
                            }

                            let projected = self.elim_env().record_proj(scrut_value.clone(), n);
                            update_telescope(projected);
                        }

                        unreachable!()
                    }
                    Value::Error => (Expr::Error, Type::Error),
                    _ => {
                        let scrut_type = self.quote_env().quote(&scrut_type);
                        let scrut_type = self.pretty(&scrut_type);
                        self.report_diagnostic(
                            Diagnostic::error()
                                .with_message(format!("Expected record, found `{scrut_type}`"))
                                .with_labels(vec![Label::primary(self.file_id, name.range)]),
                        );
                        (Expr::Error, Type::Error)
                    }
                }
            }
        }
    }

    /// Wrap an expr in fresh implicit applications that correspond to implicit
    /// parameters in the type provided.
    fn insert_implicit_apps(
        &mut self,
        fun_range: TextRange,
        mut expr: Expr<'core>,
        mut r#type: Type<'core>,
    ) -> (Expr<'core>, Type<'core>) {
        loop {
            r#type = self.elim_env().update_metas(&r#type);
            match r#type {
                Value::FunType { param, body } if param.plicity.is_implicit() => {
                    let source = MetaSource::ImplicitArg {
                        range: fun_range,
                        name: param.name,
                    };
                    let arg_expr = self.push_unsolved_expr(source, param.r#type.clone());
                    let arg_value = self.eval_env().eval(&arg_expr);

                    let (fun, arg_expr) = self.bump.alloc((expr, arg_expr));

                    let arg = FunArg::new(param.plicity, arg_expr as &_);
                    expr = Expr::FunApp { fun, arg };
                    r#type = self.elim_env().apply_closure(body, arg_value);
                }
                _ => break,
            }
        }
        (expr, r#type)
    }

    fn synth_fun_type(
        &mut self,
        surface_params: &'surface [Located<surface::FunParam<'surface>>],
        surface_body: &'surface Located<surface::Expr<'surface>>,
    ) -> (Expr<'core>, Type<'core>) {
        match surface_params.split_first() {
            None => {
                let body = self.check_expr_is_type(surface_body);
                (body, Type::TYPE)
            }
            Some((surface_param, surface_params)) => {
                let (pat, param, param_type) = self.synth_param(surface_param);

                let body_expr = {
                    let local_len = self.local_env.len();
                    let var = Expr::LocalVar(RelativeVar::default());
                    let bindings = self.destruct_pat(&pat, &var, &param_type, true);

                    self.local_env.push_param(param.name, param_type);
                    self.push_let_bindings(&bindings);
                    let (body_expr, _) = self.synth_fun_type(surface_params, surface_body);
                    self.local_env.truncate(local_len);

                    Expr::lets(self.bump, &bindings, body_expr)
                };

                let core_expr = Expr::FunType {
                    param,
                    body: self.bump.alloc(body_expr),
                };
                (core_expr, Type::TYPE)
            }
        }
    }

    fn synth_fun_lit(
        &mut self,
        surface_params: &'surface [Located<surface::FunParam<'surface>>],
        surface_body: &'surface Located<surface::Expr<'surface>>,
    ) -> (Expr<'core>, Type<'core>) {
        match surface_params.split_first() {
            None => self.synth_expr(surface_body),
            Some((surface_param, surface_params)) => {
                let (pat, param, param_type) = self.synth_param(surface_param);

                let (body_expr, body_type) = {
                    let local_len = self.local_env.len();
                    let var = Expr::LocalVar(RelativeVar::default());
                    let bindings = self.destruct_pat(&pat, &var, &param_type, true);

                    self.local_env.push_param(param.name, param_type.clone());
                    self.push_let_bindings(&bindings);
                    let (body_expr, body_type) = self.synth_fun_lit(surface_params, surface_body);
                    let body_type = self.quote_env().quote(&body_type);
                    self.local_env.truncate(local_len);

                    let body_expr = Expr::lets(self.bump, &bindings, body_expr);
                    let body_type = Expr::lets(self.bump, &bindings, body_type);

                    (body_expr, body_type)
                };

                let core_expr = Expr::FunLit {
                    param,
                    body: self.bump.alloc(body_expr),
                };
                let core_type = Value::FunType {
                    param: FunParam::new(param.plicity, param.name, self.bump.alloc(param_type)),
                    body: Closure::new(self.local_env.values.clone(), self.bump.alloc(body_type)),
                };
                (core_expr, core_type)
            }
        }
    }

    pub fn check_expr_is_type(
        &mut self,
        surface_expr: &'surface Located<surface::Expr<'surface>>,
    ) -> Expr<'core> {
        self.check_expr(surface_expr, &Type::TYPE)
    }

    pub fn check_expr(
        &mut self,
        surface_expr: &'surface Located<surface::Expr<'surface>>,
        expected: &Type<'core>,
    ) -> Expr<'core> {
        let expected = self.elim_env().update_metas(expected);
        match surface_expr.data {
            surface::Expr::Error => Expr::Error,
            surface::Expr::Hole => {
                let range = surface_expr.range;
                let expr = self.push_unsolved_expr(MetaSource::HoleExpr { range }, expected);
                expr
            }
            surface::Expr::Paren(expr) => self.check_expr(expr, &expected),
            surface::Expr::Let {
                rec: surface::Rec::Nonrec,
                binding,
                body,
            } => {
                let (expr, ()) = self.elab_let(&binding, body, |this, body| {
                    let body = this.check_expr(body, &expected);
                    (body, ())
                });
                expr
            }
            surface::Expr::Let {
                rec: surface::Rec::Rec,
                binding,
                body,
            } => {
                let (expr, ()) = self.elab_letrec(&binding, body, |this, body| {
                    let body = this.check_expr(body, &expected);
                    (body, ())
                });
                expr
            }
            surface::Expr::If { cond, then, r#else } => {
                let cond = self.check_expr(cond, &Type::BOOL);
                let then = self.check_expr(then, &expected);
                let r#else = self.check_expr(r#else, &expected);
                let (cond, then, r#else) = self.bump.alloc((cond, then, r#else));
                Expr::MatchBool { cond, then, r#else }
            }
            surface::Expr::Match { scrut, cases } => {
                self.check_match_expr(surface_expr.range, scrut, cases, &expected)
            }
            surface::Expr::FunLit { params, body } => self.check_fun_lit(params, body, &expected),

            surface::Expr::ListLit(surface_exprs) => {
                let Type::Neutral(Head::Prim(Prim::List), args) = &expected else {
                    return self.synth_and_convert_expr(surface_expr, &expected);
                };
                let &[Elim::FunApp(FunArg {
                    plicity: Plicity::Explicit,
                    expr: ref elem_type,
                })] = args.as_ref()
                else {
                    return self.synth_and_convert_expr(surface_expr, &expected);
                };

                let mut exprs = SliceVec::new(self.bump, surface_exprs.len());
                for surface_expr in surface_exprs {
                    let expr = self.check_expr(surface_expr, elem_type);
                    exprs.push(expr);
                }

                Expr::ListLit(exprs.into())
            }
            surface::Expr::TupleLit(surface_exprs) if expected.is_type() => {
                let len = self.local_env.len();
                let mut type_fields = SliceVec::new(self.bump, surface_exprs.len());
                for (index, expr) in surface_exprs.iter().enumerate() {
                    let name = Symbol::tuple_index(index);
                    let r#type = self.check_expr_is_type(expr);
                    let r#type_value = self.eval_env().eval(&r#type);
                    self.local_env.push_param(None, type_value);
                    type_fields.push((name, r#type));
                }
                self.local_env.truncate(len);
                Expr::RecordType(r#type_fields.into())
            }
            surface::Expr::RecordLit(surface_fields) => {
                let Value::RecordType(telescope) = &expected else {
                    return self.synth_and_convert_expr(surface_expr, &expected);
                };

                if !pion_util::slice_eq_by_key2(
                    surface_fields,
                    telescope.fields,
                    |field| field.data.name.data,
                    |(n, _)| *n,
                ) {
                    return self.synth_and_convert_expr(surface_expr, &expected);
                }

                let mut telescope = telescope.clone();
                let mut expr_fields = SliceVec::new(self.bump, surface_fields.len());
                for surface_field in surface_fields {
                    let (name, r#type, update_telescope) =
                        self.elim_env().split_telescope(&mut telescope).unwrap();
                    let expr = self.check_expr(&surface_field.data.expr, &r#type);
                    let value = self.eval_env().eval(&expr);
                    update_telescope(value);
                    expr_fields.push((name, expr));
                }
                Expr::RecordLit(expr_fields.into())
            }

            // list cases explicitly instead of using `_` so that new cases are not forgotten when
            // new expression variants are added
            surface::Expr::Lit(..)
            | surface::Expr::LocalVar { .. }
            | surface::Expr::Ann { .. }
            | surface::Expr::FunArrow { .. }
            | surface::Expr::FunType { .. }
            | surface::Expr::FunApp { .. }
            | surface::Expr::TupleLit(..)
            | surface::Expr::RecordType(..)
            | surface::Expr::RecordProj { .. } => {
                self.synth_and_convert_expr(surface_expr, &expected)
            }
        }
    }

    fn check_fun_lit(
        &mut self,
        surface_params: &'surface [Located<surface::FunParam<'surface>>],
        surface_body: &'surface Located<surface::Expr<'surface>>,
        expected: &Type<'core>,
    ) -> Expr<'core> {
        let [surface_param, rest_params @ ..] = surface_params else {
            return self.check_expr(surface_body, expected);
        };

        match expected {
            // If an implicit function is expected, try to generalize the
            // function literal by wrapping it in an implicit function
            Value::FunType {
                param: expected_param,
                body: expected_body,
            } if surface_param.data.plicity.is_explicit()
                && expected_param.plicity.is_implicit() =>
            {
                let r#type = self.quote_env().quote(expected_param.r#type);

                let var = self.local_env.next_var();
                self.local_env
                    .push_param(expected_param.name, expected_param.r#type.clone());
                let expected = self.elim_env().apply_closure(expected_body.clone(), var);
                let body = self.check_fun_lit(surface_params, surface_body, &expected);
                self.local_env.pop();

                let (r#type, body) = self.bump.alloc((r#type, body));
                let param =
                    FunParam::new(expected_param.plicity, expected_param.name, r#type as &_);
                Expr::FunLit { param, body }
            }
            Value::FunType {
                param: expected_param,
                body: expected_body,
            } if surface_param.data.plicity == expected_param.plicity => {
                let (pat, param) = self.check_param(surface_param, expected_param.r#type);

                let body_expr = {
                    let local_len = self.local_env.len();
                    let var = Expr::LocalVar(RelativeVar::default());
                    let bindings = self.destruct_pat(&pat, &var, expected_param.r#type, true);

                    let arg_value = self.local_env.next_var();
                    self.local_env
                        .push_param(param.name, expected_param.r#type.clone());
                    self.push_let_bindings(&bindings);
                    let expected = self
                        .elim_env()
                        .apply_closure(expected_body.clone(), arg_value);
                    let body_expr = self.check_fun_lit(rest_params, surface_body, &expected);
                    self.local_env.truncate(local_len);

                    Expr::lets(self.bump, &bindings, body_expr)
                };

                let core_expr = Expr::FunLit {
                    param,
                    body: self.bump.alloc(body_expr),
                };
                core_expr
            }
            _ => {
                let (expr, r#type) = self.synth_fun_lit(surface_params, surface_body);
                let range = surface_param.range.cover(surface_body.range);
                self.convert_expr(range, expr, r#type, expected)
            }
        }
    }

    fn synth_and_convert_expr(
        &mut self,
        surface_expr: &'surface Located<surface::Expr<'surface>>,
        expected: &Type<'core>,
    ) -> Expr<'core> {
        let range = surface_expr.range;
        let (expr, r#type) = self.synth_expr(surface_expr);
        self.convert_expr(range, expr, r#type, expected)
    }

    fn convert_expr(
        &mut self,
        range: TextRange,
        expr: Expr<'core>,
        from: Type<'core>,
        to: &Type<'core>,
    ) -> Expr<'core> {
        // Attempt to specialize exprs with freshly inserted implicit
        // arguments if an explicit function was expected.
        let (expr, from) = match (expr, to) {
            (Expr::FunLit { .. }, _) => (expr, from),
            (_, Type::FunType { param, .. }) if param.plicity.is_explicit() => {
                self.insert_implicit_apps(range, expr, from)
            }
            _ => (expr, from),
        };

        match self.unify_env().unify(&from, to) {
            Ok(()) => expr,
            Err(error) => {
                let from = self.quote_env().quote(&from);
                let to = self.quote_env().quote(to);

                let found = self.pretty(&from);
                let expected = self.pretty(&to);

                let diagnostic = error.to_diagnostic(self.file_id, range, &expected, &found);
                self.report_diagnostic(diagnostic);
                Expr::Error
            }
        }
    }
}
