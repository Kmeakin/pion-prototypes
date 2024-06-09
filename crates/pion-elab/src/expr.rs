use std::num::NonZero;

use ecow::eco_vec;
use pion_core::env::RelativeVar;
use pion_core::prim::Prim;
use pion_core::semantics::{Closure, Elim, Head, Telescope, Type, Value};
use pion_core::syntax::{Expr, FunArg, FunParam, Lit, Plicity};
use pion_surface::syntax::{self as surface, Located};
use pion_symbol::Symbol;
use pion_util::location::Location;
use pion_util::numeric_conversions::TruncateFrom;
use pion_util::slice_vec::SliceVec;
use text_size::TextRange;

use super::{Elaborator, MetaSource};
use crate::diagnostics;

impl<'handler, 'core, 'text, 'surface> Elaborator<'handler, 'core, 'text> {
    pub fn synth_lit(
        &mut self,
        surface_lit: &Located<surface::Lit>,
    ) -> (Result<Lit, ()>, Type<'core>) {
        let range = surface_lit.range;
        let (lit, r#type) = match surface_lit.data {
            surface::Lit::Bool(b) => (Ok(Lit::Bool(b)), Type::BOOL),
            surface::Lit::Int(int) => match self.synth_int_lit(Located::new(range, int)) {
                Ok(value) => (Ok(Lit::Int(value)), Type::INT),
                Err(error) => {
                    let lit_loc = Location::new(self.file_id, range);
                    diagnostics::invalid_integer_literal(self, error, lit_loc);
                    (Err(()), Type::INT)
                }
            },
        };
        (lit, r#type)
    }

    fn synth_int_lit(
        &self,
        int: Located<surface::IntLit>,
    ) -> Result<u32, lexical_parse_integer::Error> {
        use lexical_parse_integer::{FromLexicalWithOptions, NumberFormatBuilder};

        const COMMON_OPTS: NumberFormatBuilder = NumberFormatBuilder::new()
            .digit_separator(NonZero::new(b'_'))
            .integer_internal_digit_separator(true);

        const DEC_OPTS: u128 = COMMON_OPTS.radix(10).build();
        const BIN_OPTS: u128 = COMMON_OPTS
            .radix(2)
            .base_prefix(NonZero::new(b'b'))
            .case_sensitive_base_prefix(false)
            .build();
        const HEX_OPTS: u128 = COMMON_OPTS
            .radix(16)
            .base_prefix(NonZero::new(b'x'))
            .case_sensitive_base_prefix(false)
            .build();

        let text = &self.text[int.range];
        let bytes = text.as_bytes();
        let opts = lexical_parse_integer::Options::default();

        match int.data {
            surface::IntLit::Dec => u32::from_lexical_with_options::<DEC_OPTS>(bytes, &opts),
            surface::IntLit::Bin => u32::from_lexical_with_options::<BIN_OPTS>(bytes, &opts),
            surface::IntLit::Hex => u32::from_lexical_with_options::<HEX_OPTS>(bytes, &opts),
        }
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
            surface::Expr::VarRef(Located { data: name, .. }) => {
                if let Some(var) = self.env.locals.lookup(name) {
                    let r#type = self.env.locals.types.get_relative(var).unwrap().clone();
                    return (Expr::LocalVar(var), r#type);
                }

                if let Some(prim) = Prim::from_symbol(name) {
                    let r#type = prim.r#type();
                    return (Expr::Prim(prim), r#type);
                }

                let var_loc = Location::new(self.file_id, surface_expr.range);
                diagnostics::unbound_local_var(self, name, var_loc);
                (Expr::Error, Type::Error)
            }
            surface::Expr::Hole => {
                let range = surface_expr.range;
                let r#type = self.push_unsolved_type(MetaSource::HoleType { range });
                let expr = self.push_unsolved_expr(MetaSource::HoleExpr { range }, r#type.clone());
                (expr, r#type)
            }
            surface::Expr::Paren(expr) => self.synth_expr(expr),
            surface::Expr::Ann(expr, r#type) => {
                let r#type = self.check_expr_is_type(r#type);
                let r#type = self.eval_env().eval(&r#type);
                let expr = self.check_expr(expr, &r#type);
                (expr, r#type)
            }
            surface::Expr::Do(block) => self.synth_block(&block),
            surface::Expr::If(cond, then, r#else) => {
                let cond = self.check_expr(cond, &Type::BOOL);
                let (then, then_type) = self.synth_expr(then);
                let r#else = self.check_expr(r#else, &then_type);
                let (cond, then, r#else) = self.bump.alloc((cond, then, r#else));
                (Expr::MatchBool { cond, then, r#else }, then_type)
            }
            surface::Expr::Match(scrut, cases) => {
                let range = surface_expr.range;
                let source = MetaSource::MatchResultType { range };
                let r#type = self.push_unsolved_type(source);
                let expr = self.check_match_expr(range, scrut, cases, &r#type);
                (expr, r#type)
            }
            surface::Expr::FunArrow(plicity, lhs, rhs) => {
                let param_type = self.check_expr_is_type(lhs);
                let body = {
                    let param_type_value = self.eval_env().eval(&param_type);
                    self.env.locals.push_param(None, param_type_value);
                    let body = self.check_expr_is_type(rhs);
                    self.env.locals.pop();
                    body
                };
                let (param_type, body) = self.bump.alloc((param_type, body));
                let core_expr = Expr::FunType {
                    param: FunParam::new(plicity.into(), None, param_type),
                    body,
                };
                (core_expr, Type::TYPE)
            }
            surface::Expr::FunType(params, body) => self.synth_fun_type(params, body),
            surface::Expr::FunLit(params, body) => self.synth_fun_lit(params, body),
            surface::Expr::FunApp(fun, surface_args) => {
                let (mut expr, fun_type) = self.synth_expr(fun);
                let mut r#type = fun_type.clone();

                for (arity, surface_arg) in surface_args.iter().enumerate() {
                    r#type = self.elim_env().update_metas(&r#type);

                    if surface_arg.data.plicity == Plicity::Explicit {
                        (expr, r#type) = self.insert_implicit_apps(surface_arg.range, expr, r#type);
                    }

                    match r#type {
                        Value::FunType { param, body }
                            if param.plicity == surface_arg.data.plicity =>
                        {
                            let arg_expr = self.check_expr(surface_arg.data.expr, param.r#type);
                            let arg_value = self.eval_env().eval(&arg_expr);
                            let (fun_expr, arg_expr) = self.bump.alloc((expr, arg_expr));
                            expr = Expr::FunApp {
                                fun: &*fun_expr,
                                arg: FunArg::new(param.plicity, &*arg_expr),
                            };
                            r#type = self.elim_env().apply_closure(body, arg_value);
                        }
                        Value::FunType { param, .. } => {
                            let arg_loc = Location::new(self.file_id, surface_arg.range);
                            let fun_loc = Location::new(self.file_id, fun.range);
                            diagnostics::fun_app_plicity_mismatch(
                                self,
                                surface_arg.data.plicity,
                                param.plicity,
                                &fun_type,
                                arg_loc,
                                fun_loc,
                            );
                            return (Expr::Error, Type::Error);
                        }
                        Value::Error => return (Expr::Error, Type::Error),
                        _ if arity == 0 => {
                            let fun_loc = Location::new(self.file_id, fun.range);
                            diagnostics::fun_app_not_fun(self, &r#type, fun_loc);
                            return (Expr::Error, Type::Error);
                        }
                        _ => {
                            let fun_loc = Location::new(self.file_id, fun.range);
                            let arg_loc = Location::new(self.file_id, surface_arg.range);
                            diagnostics::fun_app_too_many_args(
                                self,
                                arity,
                                surface_args.len(),
                                &fun_type,
                                fun_loc,
                                arg_loc,
                            );
                            return (Expr::Error, Type::Error);
                        }
                    }
                }

                (expr, r#type)
            }
            surface::Expr::ListLit(surface_exprs) => {
                let Some((first_surface_expr, rest_surface_exprs)) = surface_exprs.split_first()
                else {
                    let elem_type = self.push_unsolved_type(MetaSource::ListElemType {
                        range: surface_expr.range,
                    });
                    let r#type = Type::Neutral(
                        Head::Prim(Prim::List),
                        eco_vec![Elim::FunApp(FunArg::explicit(elem_type))],
                    );
                    return (Expr::ListLit(&[]), r#type);
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
                    let name = Symbol::tuple_index(u32::truncate_from(index));
                    expr_fields.push((name, expr));
                    type_fields.push((name, self.quote_env().quote_at(&r#type, index)));
                }

                let telescope = Telescope::new(self.env.locals.values.clone(), type_fields.into());
                (
                    Expr::RecordLit(expr_fields.into()),
                    Type::RecordType(telescope),
                )
            }
            surface::Expr::RecordType(surface_fields) => {
                let mut type_fields = SliceVec::new(self.bump, surface_fields.len());
                let local_len = self.env.locals.len();

                for surface_field in surface_fields {
                    let name = surface_field.data.name.data;
                    if let Some(index) = type_fields.iter().position(|(n, _)| *n == name) {
                        let duplicate_loc =
                            Location::new(self.file_id, surface_field.data.name.range);
                        let first_loc =
                            Location::new(self.file_id, surface_fields[index].data.name.range);
                        diagnostics::duplicate_record_field(self, name, duplicate_loc, first_loc);
                        continue;
                    }

                    let r#type = self.check_expr_is_type(&surface_field.data.r#type);
                    let r#type_value = self.eval_env().eval(&r#type);
                    type_fields.push((name, r#type));
                    self.env.locals.push_param(Some(name), r#type_value);
                }
                self.env.locals.truncate(local_len);

                (Expr::RecordType(type_fields.into()), Type::TYPE)
            }
            surface::Expr::RecordLit(surface_fields) => {
                let mut expr_fields = SliceVec::new(self.bump, surface_fields.len());
                let mut type_fields = SliceVec::new(self.bump, surface_fields.len());

                for surface_field in surface_fields {
                    let name = surface_field.data.name.data;
                    if let Some(index) = expr_fields.iter().position(|(n, _)| *n == name) {
                        let duplicate_loc =
                            Location::new(self.file_id, surface_field.data.name.range);
                        let first_loc =
                            Location::new(self.file_id, surface_fields[index].data.name.range);
                        diagnostics::duplicate_record_field(self, name, duplicate_loc, first_loc);
                        continue;
                    }

                    let (expr, r#type) = self.synth_expr(&surface_field.data.expr);
                    let r#type = self.quote_env().quote_at(&r#type, expr_fields.len());
                    expr_fields.push((name, expr));
                    type_fields.push((name, r#type));
                }

                let telescope = Telescope::new(self.env.locals.values.clone(), type_fields.into());
                (
                    Expr::RecordLit(expr_fields.into()),
                    Type::RecordType(telescope),
                )
            }
            surface::Expr::RecordProj(scrut, name) => {
                let (scrut_expr, scrut_type) = self.synth_expr(scrut);

                let scrut_type = self.elim_env().update_metas(&scrut_type);

                match scrut_type {
                    Value::RecordType(mut telescope) => {
                        let Some(_) = telescope.fields.iter().find(|(n, _)| *n == name.data) else {
                            let loc = Location::new(self.file_id, name.range);
                            diagnostics::field_not_found(self, name.data, loc);
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
                        let scrut_loc = Location::new(self.file_id, scrut.range);
                        diagnostics::record_proj_not_record(self, &scrut_type, scrut_loc);
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

                    let arg = FunArg::new(param.plicity, &*arg_expr);
                    expr = Expr::FunApp { fun, arg };
                    r#type = self.elim_env().apply_closure(body, arg_value);
                }
                _ => break,
            }
        }
        (expr, r#type)
    }

    // FIXME: check patterns for exhaustiveness
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
                    let local_len = self.env.locals.len();
                    let var = Expr::LocalVar(RelativeVar::default());
                    let bindings = self.destruct_pat(&pat, &var, &param_type, true);

                    self.env.locals.push_param(param.name, param_type);
                    self.push_let_bindings(&bindings);
                    let (body_expr, _) = self.synth_fun_type(surface_params, surface_body);
                    self.env.locals.truncate(local_len);

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

    // FIXME: check patterns for exhaustiveness
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
                    let local_len = self.env.locals.len();
                    let var = Expr::LocalVar(RelativeVar::default());
                    let bindings = self.destruct_pat(&pat, &var, &param_type, true);

                    self.env.locals.push_param(param.name, param_type.clone());
                    self.push_let_bindings(&bindings);
                    let (body_expr, body_type) = self.synth_fun_lit(surface_params, surface_body);
                    let body_type = self.quote_env().quote(&body_type);
                    self.env.locals.truncate(local_len);

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
                    body: Closure::new(self.env.locals.values.clone(), self.bump.alloc(body_type)),
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
            surface::Expr::Do(block) => self.check_block(surface_expr.range, &block, &expected),
            surface::Expr::If(cond, then, r#else) => {
                let cond = self.check_expr(cond, &Type::BOOL);
                let then = self.check_expr(then, &expected);
                let r#else = self.check_expr(r#else, &expected);
                let (cond, then, r#else) = self.bump.alloc((cond, then, r#else));
                Expr::MatchBool { cond, then, r#else }
            }
            surface::Expr::Match(scrut, cases) => {
                self.check_match_expr(surface_expr.range, scrut, cases, &expected)
            }
            surface::Expr::FunLit(params, body) => self.check_fun_lit(params, body, &expected),

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
                let len = self.env.locals.len();
                let mut type_fields = SliceVec::new(self.bump, surface_exprs.len());
                for (index, expr) in surface_exprs.iter().enumerate() {
                    let name = Symbol::tuple_index(u32::truncate_from(index));
                    let r#type = self.check_expr_is_type(expr);
                    let r#type_value = self.eval_env().eval(&r#type);
                    self.env.locals.push_param(None, type_value);
                    type_fields.push((name, r#type));
                }
                self.env.locals.truncate(len);
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
            | surface::Expr::VarRef { .. }
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

    // FIXME: check patterns for exhaustiveness
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

                let var = self.env.locals.next_var();
                self.env
                    .locals
                    .push_param(expected_param.name, expected_param.r#type.clone());
                let expected = self.elim_env().apply_closure(expected_body.clone(), var);
                let body = self.check_fun_lit(surface_params, surface_body, &expected);
                self.env.locals.pop();

                let (r#type, body) = self.bump.alloc((r#type, body));
                let param = FunParam::new(expected_param.plicity, expected_param.name, &*r#type);
                Expr::FunLit { param, body }
            }
            Value::FunType {
                param: expected_param,
                body: expected_body,
            } if surface_param.data.plicity == expected_param.plicity => {
                let (pat, param) = self.check_param(surface_param, expected_param.r#type);

                let body_expr = {
                    let local_len = self.env.locals.len();
                    let var = Expr::LocalVar(RelativeVar::default());
                    let bindings = self.destruct_pat(&pat, &var, expected_param.r#type, true);

                    let arg_value = self.env.locals.next_var();
                    self.env
                        .locals
                        .push_param(param.name, expected_param.r#type.clone());
                    self.push_let_bindings(&bindings);
                    let expected = self
                        .elim_env()
                        .apply_closure(expected_body.clone(), arg_value);
                    let body_expr = self.check_fun_lit(rest_params, surface_body, &expected);
                    self.env.locals.truncate(local_len);

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

    pub(super) fn convert_expr(
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
                let loc = Location::new(self.file_id, range);
                diagnostics::unable_to_unify(self, error, &from, to, loc);
                Expr::Error
            }
        }
    }
}
