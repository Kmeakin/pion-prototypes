use codespan_reporting::diagnostic::{Diagnostic, Label};
use common::env::{EnvLen, RelativeVar, SharedEnv, UniqueEnv};
use common::Symbol;

use crate::core::semantics::{self, Type, Value};
use crate::core::syntax::{Const, Expr, FunParam, Prim};
use crate::surface::{self, Located};

pub struct Elaborator<'core, 'text, H, E>
where
    H: FnMut(Diagnostic<usize>) -> Result<(), E>,
{
    bump: &'core bumpalo::Bump,
    text: &'text str,
    file_id: usize,
    handler: H,

    local_env: LocalEnv<'core>,
}

#[derive(Default)]
struct LocalEnv<'core> {
    names: UniqueEnv<Option<Symbol>>,
    types: UniqueEnv<Type<'core>>,
    values: SharedEnv<Value<'core>>,
}

impl<'core> LocalEnv<'core> {
    pub fn len(&self) -> EnvLen { self.names.len() }

    pub fn push(&mut self, name: Option<Symbol>, r#type: Type<'core>, value: Value<'core>) {
        self.names.push(name);
        self.types.push(r#type);
        self.values.push(value);
    }

    pub fn push_unknown(&mut self, name: Option<Symbol>, r#type: Type<'core>) {
        let var = Value::local_var(self.values.len().to_absolute());
        self.push(name, r#type, var);
    }

    pub fn pop(&mut self) {
        self.names.pop();
        self.types.pop();
        self.values.pop();
    }

    pub fn lookup(&self, sym: Symbol) -> Option<RelativeVar> {
        self.names
            .iter()
            .rev()
            .enumerate()
            .find(|(_, name)| **name == Some(sym))
            .map(|(idx, _)| RelativeVar::from(idx))
    }

    fn next_var(&self) -> Value<'core> { Value::local_var(self.len().to_absolute()) }
}

impl<'core, 'text, H, E> Elaborator<'core, 'text, H, E>
where
    H: FnMut(Diagnostic<usize>) -> Result<(), E>,
{
    pub fn new(bump: &'core bumpalo::Bump, text: &'text str, file_id: usize, handler: H) -> Self {
        Self {
            bump,
            text,
            file_id,
            handler,

            local_env: LocalEnv::default(),
        }
    }

    fn report_diagnostic(&mut self, diagnostic: Diagnostic<usize>) -> Result<(), E> {
        (self.handler)(diagnostic)
    }

    pub fn eval(&mut self, expr: &Expr<'core>) -> Value<'core> {
        crate::core::semantics::eval(self.bump, &mut self.local_env.values, expr)
    }

    pub fn quote(&mut self, value: &Value<'core>) -> Expr<'core> {
        crate::core::semantics::quote(self.bump, self.local_env.values.len(), value)
    }

    pub fn normalize(&mut self, expr: &Expr<'core>) -> Expr<'core> {
        crate::core::semantics::normalize(self.bump, &mut self.local_env.values, expr)
    }

    pub fn pretty(&mut self, expr: &Expr<'core>) -> String {
        let printer = crate::core::print::Printer::new(self.bump, Default::default());
        let doc = printer.expr(&mut self.local_env.names, expr).into_doc();
        doc.pretty(usize::MAX).to_string()
    }

    pub fn synth_expr<'surface>(
        &mut self,
        surface_expr: &'surface Located<surface::Expr<'surface>>,
    ) -> Result<(Expr<'core>, Type<'core>), E> {
        match surface_expr.data {
            surface::Expr::Error => Ok((Expr::Error, Type::Error)),
            surface::Expr::Const(r#const) => {
                let mut parse_int = |base| {
                    let text = &self.text[surface_expr.range];
                    match u32::from_str_radix(text, base) {
                        Ok(int) => Ok(Expr::Const(Const::Int(int))),
                        Err(error) => {
                            self.report_diagnostic(
                                Diagnostic::error()
                                    .with_message(format!("Invalid integer literal: {error}"))
                                    .with_labels(vec![Label::primary(
                                        self.file_id,
                                        surface_expr.range,
                                    )]),
                            )?;
                            Ok(Expr::Error)
                        }
                    }
                };
                let (expr, r#type) = match r#const {
                    surface::Const::Bool(b) => (Expr::Const(Const::Bool(b)), Type::BOOL_TYPE),
                    surface::Const::DecInt => (parse_int(10)?, Type::INT_TYPE),
                    surface::Const::BinInt => (parse_int(2)?, Type::INT_TYPE),
                    surface::Const::HexInt => (parse_int(16)?, Type::INT_TYPE),
                };
                Ok((expr, r#type))
            }
            surface::Expr::LocalVar => {
                let text = &self.text[surface_expr.range];
                let name = Symbol::from(text);

                if let Some(var) = self.local_env.lookup(name) {
                    let r#type = self.local_env.types.get_relative(var).unwrap().clone();
                    return Ok((Expr::LocalVar { var }, r#type));
                }

                match text {
                    "Type" => return Ok((Expr::Prim(Prim::Type), Type::TYPE)),
                    "Int" => return Ok((Expr::Prim(Prim::IntType), Type::TYPE)),
                    "Bool" => return Ok((Expr::Prim(Prim::BoolType), Type::TYPE)),
                    _ => {}
                }

                self.report_diagnostic(
                    Diagnostic::error()
                        .with_message(format!("Unbound variable: {text}"))
                        .with_labels(vec![Label::primary(self.file_id, surface_expr.range)]),
                )?;
                Ok((Expr::Error, Type::Error))
            }
            surface::Expr::Paren { expr } => self.synth_expr(expr),
            surface::Expr::Let {
                pat,
                r#type,
                init,
                body,
            } => {
                let (name, r#type) = self.synth_ann_pat(pat, r#type)?;
                let r#type_expr = self.quote(&r#type);
                let init_expr = self.check_expr(init, &r#type)?;
                let init_value = self.eval(&init_expr);
                let (body_expr, body_type) = {
                    self.local_env.push(name, r#type, init_value);
                    let (body_expr, body_type) = self.synth_expr(body)?;
                    self.local_env.pop();
                    (body_expr, body_type)
                };
                let (r#type, init, body) = self.bump.alloc((r#type_expr, init_expr, body_expr));
                let core_expr = Expr::Let {
                    name,
                    r#type,
                    init,
                    body,
                };
                Ok((core_expr, body_type))
            }
            surface::Expr::FunArrow { lhs, rhs } => {
                let param_type = self.check_expr_is_type(lhs)?;
                let body = {
                    let param_type_value = self.eval(&param_type);
                    self.local_env.push_unknown(None, param_type_value);
                    let body = self.check_expr_is_type(rhs)?;
                    self.local_env.pop();
                    body
                };
                let (param_type, body) = self.bump.alloc((param_type, body));
                let core_expr = Expr::FunType {
                    param: FunParam::new(None, param_type),
                    body,
                };
                Ok((core_expr, Type::TYPE))
            }
            surface::Expr::FunType { param, body } => {
                let (param, param_type) = self.synth_param(param)?;
                let body_expr = {
                    self.local_env.push_unknown(param.name, param_type);
                    let body_expr = self.check_expr_is_type(body)?;
                    self.local_env.pop();
                    body_expr
                };
                let core_expr = Expr::FunType {
                    param,
                    body: self.bump.alloc(body_expr),
                };
                Ok((core_expr, Type::TYPE))
            }
            surface::Expr::FunLit { param, body } => {
                let (param, param_type) = self.synth_param(param)?;
                let (body_expr, body_type) = {
                    self.local_env.push_unknown(param.name, param_type);
                    let (body_expr, body_type) = self.synth_expr(body)?;
                    self.local_env.pop();
                    (body_expr, body_type)
                };
                let core_expr = Expr::FunLit {
                    param,
                    body: self.bump.alloc(body_expr),
                };
                Ok((core_expr, body_type))
            }
            surface::Expr::FunApp { fun, arg } => {
                let (fun_expr, fun_type) = self.synth_expr(fun)?;
                match fun_type {
                    Value::Error => Ok((Expr::Error, Type::Error)),
                    Value::FunType { param, body } => {
                        let arg_expr = self.check_expr(arg, param.r#type)?;
                        let arg_value = self.eval(&arg_expr);
                        let output_type = semantics::apply_closure(self.bump, body, arg_value);

                        let (fun, arg) = self.bump.alloc((fun_expr, arg_expr));
                        let core_expr = Expr::FunApp { fun, arg };
                        Ok((core_expr, output_type))
                    }
                    _ => {
                        let fun_type = self.quote(&fun_type);
                        let fun_type = self.pretty(&fun_type);
                        self.report_diagnostic(
                            Diagnostic::error()
                                .with_message(format!("Expected function, found `{fun_type}`"))
                                .with_labels(vec![Label::primary(self.file_id, fun.range)]),
                        )?;
                        Ok((Expr::Error, Type::Error))
                    }
                }
            }
        }
    }

    pub fn check_expr_is_type<'surface>(
        &mut self,
        surface_expr: &'surface Located<surface::Expr<'surface>>,
    ) -> Result<Expr<'core>, E> {
        self.check_expr(surface_expr, &Type::TYPE)
    }

    pub fn check_expr<'surface>(
        &mut self,
        surface_expr: &'surface Located<surface::Expr<'surface>>,
        expected: &Type<'core>,
    ) -> Result<Expr<'core>, E> {
        match surface_expr.data {
            surface::Expr::Error => Ok(Expr::Error),
            surface::Expr::Paren { expr } => self.check_expr(expr, expected),
            surface::Expr::Let {
                pat,
                r#type,
                init,
                body,
            } => {
                let (name, r#type) = self.synth_ann_pat(pat, r#type)?;
                let r#type_expr = self.quote(&r#type);
                let init_expr = self.check_expr(init, &r#type)?;
                let init_value = self.eval(&init_expr);
                let body_expr = {
                    self.local_env.push(name, r#type, init_value);
                    let body_expr = self.check_expr(body, expected)?;
                    self.local_env.pop();
                    body_expr
                };
                let (r#type, init, body) = self.bump.alloc((r#type_expr, init_expr, body_expr));
                let core_expr = Expr::Let {
                    name,
                    r#type,
                    init,
                    body,
                };
                Ok(core_expr)
            }
            surface::Expr::FunLit { param, body }
                if let Type::FunType {
                    param: expected_param,
                    body: expected_body,
                } = expected =>
            {
                let param = self.check_param(param, expected_param.r#type)?;
                let body_expr = {
                    let arg_value = self.local_env.next_var();
                    self.local_env
                        .push_unknown(param.name, expected_param.r#type.clone());
                    let expected_body_type = crate::core::semantics::apply_closure(
                        self.bump,
                        expected_body.clone(),
                        arg_value,
                    );
                    let body_expr = self.check_expr(body, &expected_body_type)?;
                    self.local_env.pop();
                    body_expr
                };
                let core_expr = Expr::FunLit {
                    param,
                    body: self.bump.alloc(body_expr),
                };
                Ok(core_expr)
            }
            _ => todo!("unification"),
        }
    }

    fn synth_param<'surface>(
        &mut self,
        surface_param: &'surface Located<surface::FunParam<'surface>>,
    ) -> Result<(FunParam<&'core Expr<'core>>, Type<'core>), E> {
        let surface_param = surface_param.data;
        let (name, r#type_value) =
            self.synth_ann_pat(&surface_param.pat, surface_param.r#type.as_ref())?;
        let r#type = self.quote(&r#type_value);
        Ok((FunParam::new(name, self.bump.alloc(r#type)), type_value))
    }

    fn check_param<'surface>(
        &mut self,
        surface_param: &'surface Located<surface::FunParam<'surface>>,
        expected: &Type<'core>,
    ) -> Result<FunParam<&'core Expr<'core>>, E> {
        let surface_param = surface_param.data;
        let name =
            self.check_ann_pat(&surface_param.pat, surface_param.r#type.as_ref(), expected)?;
        let r#type = self.quote(expected);
        Ok(FunParam::new(name, self.bump.alloc(r#type)))
    }

    fn synth_ann_pat<'surface>(
        &mut self,
        surface_pat: &'surface Located<surface::Pat<'surface>>,
        surface_ann: Option<&'surface Located<surface::Expr<'surface>>>,
    ) -> Result<(Option<Symbol>, Type<'core>), E> {
        match surface_ann {
            None => self.synth_pat(surface_pat),
            Some(surface_ann) => {
                let ann_expr = self.check_expr_is_type(surface_ann)?;
                let ann_value = self.eval(&ann_expr);
                let name = self.check_pat(surface_pat, &ann_value)?;
                Ok((name, ann_value))
            }
        }
    }

    fn check_ann_pat<'surface>(
        &mut self,
        surface_pat: &'surface Located<surface::Pat<'surface>>,
        surface_ann: Option<&'surface Located<surface::Expr<'surface>>>,
        expected: &Type<'core>,
    ) -> Result<Option<Symbol>, E> {
        match surface_ann {
            None => self.check_pat(surface_pat, expected),
            Some(surface_ann) => {
                let ann_expr = self.check_expr_is_type(surface_ann)?;
                let ann_value = self.eval(&ann_expr);
                let name = self.check_pat(surface_pat, &ann_value)?;
                Ok(name)
            }
        }
    }

    fn synth_pat<'surface>(
        &mut self,
        surface_pat: &'surface Located<surface::Pat<'surface>>,
    ) -> Result<(Option<Symbol>, Type<'core>), E> {
        match surface_pat.data {
            surface::Pat::Error => Ok((None, Type::Error)),
            surface::Pat::Underscore => todo!("unification"),
            surface::Pat::Ident => todo!("unification"),
            surface::Pat::Paren { pat } => self.synth_pat(pat),
        }
    }

    fn check_pat<'surface>(
        &mut self,
        surface_pat: &'surface Located<surface::Pat<'surface>>,
        expected: &Type<'core>,
    ) -> Result<Option<Symbol>, E> {
        match surface_pat.data {
            surface::Pat::Error => Ok(None),
            surface::Pat::Underscore => Ok(None),
            surface::Pat::Ident => {
                let text = &self.text[surface_pat.range];
                let symbol = Symbol::from(text);
                Ok(Some(symbol))
            }
            surface::Pat::Paren { pat } => self.check_pat(pat, expected),
        }
    }
}
