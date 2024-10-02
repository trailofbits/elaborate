use if_chain::if_chain;
use rustdoc_types::{Function, GenericBound, GenericParamDefKind, Type, WherePredicate};

pub trait FunctionExt {
    fn input_is_redeclarable(&self, i: usize) -> Option<&str>;
    fn input_is_uncloneble(&self, i: usize) -> Option<&str>;
    fn input_is_trait_bound(&self, i: usize, trait_name: &str) -> bool;
    fn input_requires_clone(&self, i: usize) -> bool;
}

const REDECLARABLE_TRAITS: &[(&str, &str)] = &[("AsFd", "as_fd"), ("AsRef", "as_ref")];

const UNCLONEABLE_TRAITS: &[&str] = &["FnOnce", "IntoIterator", "Read", "ToSocketAddrs"];

impl FunctionExt for Function {
    fn input_is_redeclarable(&self, i: usize) -> Option<&str> {
        for (trait_name, method_name) in REDECLARABLE_TRAITS {
            if self.input_is_trait_bound(i, trait_name) {
                return Some(method_name);
            }
        }
        None
    }

    fn input_is_uncloneble(&self, i: usize) -> Option<&str> {
        UNCLONEABLE_TRAITS
            .iter()
            .copied()
            .find(|trait_name| self.input_is_trait_bound(i, trait_name))
    }

    fn input_is_trait_bound(&self, i: usize, trait_name: &str) -> bool {
        let input_type = &self.decl.inputs[i].1;
        if_chain! {
            if let Type::ImplTrait(bounds) = input_type;
            if bounds.has_trait_bound_with_name(trait_name);
            then {
                return true;
            }
        }
        if_chain! {
            if let Type::Generic(name) = input_type;
            if let Some(generic_param_def) = self
                .generics
                .params
                .iter()
                .find(|generic_param_def| *name == generic_param_def.name);
            if let GenericParamDefKind::Type { bounds, .. } = &generic_param_def.kind;
            if bounds.has_trait_bound_with_name(trait_name);
            then {
                return true;
            }
        }
        if_chain! {
            if let Type::Generic(_) = input_type;
            if let Some(bounds) =
                self
                    .generics
                    .where_predicates
                    .iter()
                    .find_map(|where_predicate| {
                        if let WherePredicate::BoundPredicate { type_, bounds, .. } = where_predicate {
                            if input_type == type_ {
                                Some(bounds)
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    });
            if bounds.has_trait_bound_with_name(trait_name);
            then {
                return true;
            }
        }
        false
    }

    fn input_requires_clone(&self, i: usize) -> bool {
        let input_type = &self.decl.inputs[i].1;
        if_chain! {
            if let Type::ResolvedPath(path) = input_type;
            if path.name == "Permissions";
            then {
                true
            } else {
                false
            }
        }
    }
}

pub trait GenericBoundsExt {
    fn has_trait_bound_with_name(&self, trait_name: &str) -> bool;
}

impl GenericBoundsExt for [GenericBound] {
    fn has_trait_bound_with_name(&self, trait_name: &str) -> bool {
        self.as_ref().iter().any(|bound| {
            matches!(bound, GenericBound::TraitBound { trait_, .. } if trait_.name == trait_name)
        })
    }
}
