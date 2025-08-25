use rustdoc_types::{Function, GenericBound, GenericParamDefKind, Type, WherePredicate};

pub trait FunctionExt {
    fn input_is_redeclarable(&self, i: usize) -> Option<&str>;
    fn input_is_uncloneble(&self, i: usize) -> Option<String>;
    fn input_is_trait_bound(&self, i: usize, trait_path: &str) -> bool;
    fn input_requires_clone(&self, i: usize) -> bool;
}

const REDECLARABLE_TRAITS: &[(&str, &str)] = &[("AsFd", "as_fd"), ("AsRef", "as_ref")];

const UNCLONEABLE_TRAITS: &[&str] = &["FnOnce", "IntoIterator", "Read", "ToSocketAddrs"];

impl FunctionExt for Function {
    fn input_is_redeclarable(&self, i: usize) -> Option<&str> {
        for (trait_path, method_name) in REDECLARABLE_TRAITS {
            if self.input_is_trait_bound(i, trait_path) {
                return Some(method_name);
            }
        }
        None
    }

    fn input_is_uncloneble(&self, i: usize) -> Option<String> {
        if let Type::ResolvedPath(path) = &self.sig.inputs[i].1
            && path.path == "BorrowedCursor"
        {
            return Some(String::from("BorrowedCursor"));
        }

        UNCLONEABLE_TRAITS.iter().copied().find_map(|trait_path| {
            if self.input_is_trait_bound(i, trait_path) {
                Some(format!("impl {trait_path}"))
            } else {
                None
            }
        })
    }

    fn input_is_trait_bound(&self, i: usize, trait_path: &str) -> bool {
        let input_type = &self.sig.inputs[i].1;
        if let Type::ImplTrait(bounds) = input_type
            && bounds.has_trait_bound_with_path(trait_path)
        {
            return true;
        }
        if let Type::Generic(name) = input_type
            && let Some(generic_param_def) = self
                .generics
                .params
                .iter()
                .find(|generic_param_def| *name == generic_param_def.name)
            && let GenericParamDefKind::Type { bounds, .. } = &generic_param_def.kind
            && bounds.has_trait_bound_with_path(trait_path)
        {
            return true;
        }
        if let Type::Generic(_) = input_type
            && let Some(bounds) =
                self.generics
                    .where_predicates
                    .iter()
                    .find_map(|where_predicate| {
                        if let WherePredicate::BoundPredicate { type_, bounds, .. } =
                            where_predicate
                        {
                            if input_type == type_ {
                                Some(bounds)
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    })
            && bounds.has_trait_bound_with_path(trait_path)
        {
            return true;
        }
        false
    }

    fn input_requires_clone(&self, i: usize) -> bool {
        let input_type = &self.sig.inputs[i].1;
        if let Type::ResolvedPath(path) = input_type
            && path.path.split("::").last() == Some("Permissions")
        {
            true
        } else {
            false
        }
    }
}

pub trait GenericBoundsExt {
    fn has_trait_bound_with_path(&self, trait_path: &str) -> bool;
}

impl GenericBoundsExt for [GenericBound] {
    fn has_trait_bound_with_path(&self, trait_path: &str) -> bool {
        self.as_ref().iter().any(|bound| {
            matches!(bound, GenericBound::TraitBound { trait_, .. } if trait_.path == trait_path)
        })
    }
}
