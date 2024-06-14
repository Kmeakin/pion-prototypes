use pion_core::env::EnvLen;
use pion_core::semantics::{Telescope, Type};
use pion_core::syntax::{Expr, FunParam, LetBinding, Pat};
use pion_surface::syntax::{self as surface, Located};
use pion_symbol::{self, Symbol};
use pion_util::location::Location;
use pion_util::numeric_conversions::TruncateFrom;
use pion_util::slice_vec::SliceVec;
use text_size::TextRange;

use super::{Elaborator, MetaSource};
use crate::diagnostics;

impl<'handler, 'core, 'text, 'surface> Elaborator<'handler, 'core, 'text> {
    pub(super) fn synth_param(
        &mut self,
        surface_param: &'surface Located<surface::FunParam<'surface>>,
    ) -> (Pat<'core>, FunParam<&'core Expr<'core>>, Type<'core>) {
        let surface_param = surface_param.data;
        let (pat, r#type_value) =
            self.synth_ann_pat(&surface_param.pat, surface_param.r#type.as_ref());
        let name = pat.name();
        let r#type = self.quote_env().quote(&r#type_value);
        (
            pat,
            FunParam::new(surface_param.plicity.into(), name, self.bump.alloc(r#type)),
            type_value,
        )
    }

    pub(super) fn check_param(
        &mut self,
        surface_param: &'surface Located<surface::FunParam<'surface>>,
        expected: &Type<'core>,
    ) -> (Pat<'core>, FunParam<&'core Expr<'core>>) {
        let surface_param = surface_param.data;
        let pat = self.check_ann_pat(&surface_param.pat, surface_param.r#type.as_ref(), expected);
        let name = pat.name();
        let r#type = self.quote_env().quote(expected);
        let param = FunParam::new(
            surface_param.plicity.into(),
            name,
            &*self.bump.alloc(r#type),
        );
        (pat, param)
    }

    pub(super) fn synth_ann_pat(
        &mut self,
        surface_pat: &'surface Located<surface::Pat<'surface>>,
        surface_ann: Option<&'surface Located<surface::Expr<'surface>>>,
    ) -> (Pat<'core>, Type<'core>) {
        match surface_ann {
            None => self.synth_pat(surface_pat),
            Some(surface_ann) => {
                let ann_expr = self.check_expr_is_type(surface_ann);
                let ann_value = self.eval_env().eval(&ann_expr);
                let name = self.check_pat(surface_pat, &ann_value);
                (name, ann_value)
            }
        }
    }

    pub(super) fn check_ann_pat(
        &mut self,
        surface_pat: &'surface Located<surface::Pat<'surface>>,
        surface_ann: Option<&'surface Located<surface::Expr<'surface>>>,
        expected: &Type<'core>,
    ) -> Pat<'core> {
        match surface_ann {
            None => self.check_pat(surface_pat, expected),
            Some(surface_ann) => {
                let type_expr = self.check_expr_is_type(surface_ann);
                let type_value = self.eval_env().eval(&type_expr);
                let pat = self.check_pat(surface_pat, &type_value);
                self.convert_pat(surface_pat.range, pat, &type_value, expected);
                pat
            }
        }
    }

    // FIXME: check pattern variables are linear (each bound variable appears once)
    fn synth_pat(
        &mut self,
        surface_pat: &'surface Located<surface::Pat<'surface>>,
    ) -> (Pat<'core>, Type<'core>) {
        match surface_pat.data {
            surface::Pat::Error => (Pat::Error, Type::Error),
            surface::Pat::Underscore => {
                let range = surface_pat.range;
                let name = None;
                let source = MetaSource::PatType { range, name };
                let r#type = self.push_unsolved_type(source);
                (Pat::Underscore, r#type)
            }
            surface::Pat::Var(Located { range, data: name }) => {
                let source = MetaSource::PatType {
                    range,
                    name: Some(name),
                };
                let r#type = self.push_unsolved_type(source);
                (Pat::Ident(name), r#type)
            }
            surface::Pat::Paren(pat) => self.synth_pat(pat),
            surface::Pat::Lit(lit) => {
                let (lit, r#type) = self.synth_lit(&lit);
                let pat = match lit {
                    Ok(lit) => Pat::Lit(lit),
                    Err(()) => Pat::Error,
                };
                (pat, r#type)
            }
            surface::Pat::TupleLit(pats) => {
                let mut pat_fields = SliceVec::new(self.bump, pats.len());
                let mut type_fields = SliceVec::new(self.bump, pats.len());
                for (index, pat) in pats.iter().enumerate() {
                    let name = Symbol::tuple_index(u32::truncate_from(index));
                    let (pat, r#type) = self.synth_pat(pat);
                    pat_fields.push((name, pat));
                    type_fields.push((name, self.quote_env().quote_at(&r#type, index)));
                }

                let telescope = Telescope::new(self.env.locals.values.clone(), type_fields.into());
                let r#type = Type::RecordType(telescope);
                (Pat::RecordLit(pat_fields.into()), r#type)
            }
            surface::Pat::RecordLit(surface_fields) => {
                let mut pat_fields = SliceVec::new(self.bump, surface_fields.len());
                let mut type_fields = SliceVec::new(self.bump, surface_fields.len());

                for surface_field in surface_fields {
                    let name = surface_field.data.name.data;

                    if let Some(index) = pat_fields.iter().position(|(n, _)| *n == name) {
                        let duplicate_loc =
                            Location::new(self.file_id, surface_field.data.name.range);
                        let first_loc =
                            Location::new(self.file_id, surface_fields[index].data.name.range);
                        diagnostics::duplicate_record_field(self, name, duplicate_loc, first_loc);
                        continue;
                    }

                    let (pat, r#type) = self.synth_pat(&surface_field.data.pat);
                    let r#type = self.quote_env().quote_at(&r#type, pat_fields.len());
                    pat_fields.push((name, pat));
                    type_fields.push((name, r#type));
                }

                let telescope = Telescope::new(self.env.locals.values.clone(), type_fields.into());
                let r#type = Type::RecordType(telescope);
                (Pat::RecordLit(pat_fields.into()), r#type)
            }
            surface::Pat::Or(pats) => {
                // FIXME: check all alts bind the same set of variables
                let mut core_pats = SliceVec::new(self.bump, pats.len());
                let [first, rest @ ..] = pats else {
                    unreachable!()
                };

                let (pat, r#type) = self.synth_pat(first);
                core_pats.push(pat);
                for pat in rest {
                    let pat = self.check_pat(pat, &r#type);
                    core_pats.push(pat);
                }

                (Pat::Or(core_pats.into()), r#type)
            }
        }
    }

    pub(super) fn check_pat(
        &mut self,
        surface_pat: &'surface Located<surface::Pat<'surface>>,
        expected: &Type<'core>,
    ) -> Pat<'core> {
        match surface_pat.data {
            surface::Pat::Error => Pat::Error,
            surface::Pat::Underscore => Pat::Underscore,
            surface::Pat::Var(Located { data: name, .. }) => Pat::Ident(name),
            surface::Pat::Paren(pat) => self.check_pat(pat, expected),
            surface::Pat::TupleLit(surface_fields) => {
                let Type::RecordType(telescope) = &expected else {
                    return self.synth_and_convert_pat(surface_pat, expected);
                };

                if !Symbol::are_tuple_field_names(telescope.fields.iter().map(|(n, _)| *n)) {
                    return self.synth_and_convert_pat(surface_pat, expected);
                }

                let mut telescope = telescope.clone();
                let mut pat_fields = SliceVec::new(self.bump, surface_fields.len());
                for surface_field in surface_fields {
                    let (name, r#type, update_telescope) =
                        self.elim_env().split_telescope(&mut telescope).unwrap();
                    let pat = self.check_pat(surface_field, &r#type);
                    pat_fields.push((name, pat));
                    update_telescope(self.env.locals.next_var());
                }

                Pat::RecordLit(pat_fields.into())
            }
            surface::Pat::RecordLit(surface_fields) => {
                let Type::RecordType(telescope) = &expected else {
                    return self.synth_and_convert_pat(surface_pat, expected);
                };

                if !pion_util::slice_eq_by_key2(
                    surface_fields,
                    telescope.fields,
                    |field| field.data.name.data,
                    |field| field.0,
                ) {
                    return self.synth_and_convert_pat(surface_pat, expected);
                }

                let mut telescope = telescope.clone();
                let mut pat_fields = SliceVec::new(self.bump, surface_fields.len());
                for surface_field in surface_fields {
                    let (name, r#type, update_telescope) =
                        self.elim_env().split_telescope(&mut telescope).unwrap();
                    let pat = self.check_pat(&surface_field.data.pat, &r#type);
                    pat_fields.push((name, pat));
                    update_telescope(self.env.locals.next_var());
                }

                Pat::RecordLit(pat_fields.into())
            }
            surface::Pat::Or(pats) => {
                // FIXME: check all alts bind the same set of variables
                let mut core_pats = SliceVec::new(self.bump, pats.len());
                for pat in pats {
                    let pat = self.check_pat(pat, expected);
                    core_pats.push(pat);
                }
                Pat::Or(core_pats.into())
            }
            surface::Pat::Lit(_) => self.synth_and_convert_pat(surface_pat, expected),
        }
    }

    fn synth_and_convert_pat(
        &mut self,
        surface_pat: &'surface Located<surface::Pat<'surface>>,
        expected: &Type<'core>,
    ) -> Pat<'core> {
        let range = surface_pat.range;
        let (pat, r#type) = self.synth_pat(surface_pat);
        self.convert_pat(range, pat, &r#type, expected)
    }

    fn convert_pat(
        &mut self,
        range: TextRange,
        pat: Pat<'core>,
        from: &Type<'core>,
        to: &Type<'core>,
    ) -> Pat<'core> {
        match self.unify_env().unify(from, to) {
            Ok(()) => pat,
            Err(error) => {
                let loc = Location::new(self.file_id, range);
                diagnostics::unable_to_unify(self, error, from, to, loc);
                Pat::Error
            }
        }
    }

    pub(super) fn destruct_pat(
        &mut self,
        pat: &Pat<'core>,
        expr: &Expr<'core>,
        r#type: &Type<'core>,
        toplevel_param: bool,
    ) -> Vec<LetBinding<Expr<'core>, Expr<'core>>> {
        fn recur<'core>(
            ctx: &mut Elaborator<'_, 'core, '_>,
            pat: &Pat<'core>,
            expr: &Expr<'core>,
            r#type: &Type<'core>,
            bindings: &mut Vec<LetBinding<Expr<'core>, Expr<'core>>>,
            toplevel_param: bool,
        ) {
            match pat {
                Pat::Error | Pat::Underscore | Pat::Lit(_) => {}
                Pat::Ident(..) if toplevel_param => {}
                Pat::Ident(..) => {
                    let name = pat.name();
                    let r#type = ctx.quote_env().quote_at(r#type, bindings.len());
                    let expr = expr.shift(ctx.bump, EnvLen::from(bindings.len()));
                    bindings.push(LetBinding::new(name, r#type, expr));
                }
                Pat::RecordLit(pat_fields) => {
                    let r#type = ctx.elim_env().update_metas(r#type);
                    let Type::RecordType(mut telescope) = r#type else {
                        unreachable!("expected record type, got {type:?}")
                    };
                    for (pat_name, pat) in *pat_fields {
                        let (telescope_name, r#type, update_telescope) =
                            ctx.elim_env().split_telescope(&mut telescope).unwrap();
                        debug_assert_eq!(*pat_name, telescope_name);

                        let expr = Expr::RecordProj(ctx.bump.alloc(*expr), *pat_name);

                        recur(ctx, pat, &expr, &r#type, bindings, false);
                        update_telescope(ctx.env.locals.next_var());
                    }
                }
                Pat::Or(pats) => {
                    recur(ctx, &pats[0], expr, r#type, bindings, toplevel_param);
                }
            }
        }

        let mut bindings = Vec::new();
        recur(self, pat, expr, r#type, &mut bindings, toplevel_param);
        bindings
    }
}
