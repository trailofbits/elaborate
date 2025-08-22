use crate::TokensExt;
use anyhow::Result;
use public_api::tokens::Token;
use std::{
    collections::{BTreeMap, BTreeSet},
    fs::{File, OpenOptions, create_dir_all},
    io::Write,
};

struct TraitWrapper {
    attrs: Vec<String>,
    qualified_wrapped_trait: Vec<Token>,
}

struct StructWrapper {
    attrs: Vec<String>,
    qualified_wrapped_struct: Vec<Token>,
    must_be_sized: bool,
    must_be_write: bool,
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct Fn {
    docs: Option<String>,
    attrs: Vec<String>,
    sig: String,
    body: String,
}

#[derive(Default)]
pub struct Module {
    submodules: BTreeMap<String, Module>,
    trait_wrappers: BTreeMap<Vec<Token>, TraitWrapper>,
    struct_wrappers: BTreeMap<Vec<Token>, StructWrapper>,
    // smoelius: The keys in this `BTreeMap` are qualified trait or struct wrappers.
    fns: BTreeMap<Vec<Token>, BTreeSet<Fn>>,
}

impl Module {
    pub fn add_trait_wrapper(
        &mut self,
        path: &[&str],
        attrs: &Vec<String>,
        qualified_trait_wrapper: &[Token],
        qualified_wrapped_trait: &[Token],
    ) {
        let module = self.module_by_path(path);
        module.trait_wrappers.insert(
            qualified_trait_wrapper.to_vec(),
            TraitWrapper {
                attrs: attrs.to_owned(),
                qualified_wrapped_trait: qualified_wrapped_trait.to_owned(),
            },
        );
    }

    pub fn add_struct_wrapper(
        &mut self,
        path: &[&str],
        attrs: &Vec<String>,
        qualified_struct_wrapper: &[Token],
        qualified_wrapped_struct: &[Token],
        must_be_sized: bool,
        must_be_write: bool,
    ) {
        let module = self.module_by_path(path);
        if !module
            .struct_wrappers
            .contains_key(qualified_struct_wrapper)
        {
            module.struct_wrappers.insert(
                qualified_struct_wrapper.to_vec(),
                StructWrapper {
                    attrs: attrs.to_owned(),
                    qualified_wrapped_struct: qualified_wrapped_struct.to_owned(),
                    must_be_sized: false,
                    must_be_write: false,
                },
            );
        }
        let struct_wrapper = module
            .struct_wrappers
            .get_mut(qualified_struct_wrapper)
            .unwrap();
        struct_wrapper.must_be_sized |= must_be_sized;
        struct_wrapper.must_be_write |= must_be_write;
    }

    pub fn add_fn(
        &mut self,
        path: &[&str],
        qualified_type_wrapper: &[Token],
        docs: Option<&str>,
        attrs: &Vec<String>,
        sig: &str,
        body: &str,
    ) {
        let module = self.module_by_path(path);
        module
            .fns
            .entry(qualified_type_wrapper.to_vec())
            .or_default()
            .insert(Fn {
                docs: docs.map(ToOwned::to_owned),
                attrs: attrs.to_owned(),
                sig: sig.to_owned(),
                body: body.to_owned(),
            });
    }

    fn module_by_path(&mut self, path: &[&str]) -> &mut Module {
        let mut module = self;
        for &name in path {
            module = module.submodules.entry(name.to_owned()).or_default();
        }
        module
    }

    pub fn write(mut self, dir: impl AsRef<std::path::Path>) -> Result<()> {
        let dir = dir.as_ref();
        create_dir_all(dir)?;
        let mut file = OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .open(dir.join("mod.rs"))?;
        write_header(&mut file)?;
        writeln!(file)?;
        for submodule in self.submodules.keys() {
            writeln!(file, "pub mod {submodule};")?;
        }
        writeln!(file)?;
        self.write_trait_wrappers(&mut file)?;
        writeln!(file)?;
        self.write_struct_wrappers(&mut file)?;
        writeln!(file)?;
        self.write_fns(&mut file)?;

        for (name, submodule) in self.submodules {
            submodule.write(dir.join(name))?;
        }

        Ok(())
    }

    fn write_trait_wrappers(&mut self, file: &mut File) -> Result<()> {
        for (
            qualified_trait_wrapper,
            TraitWrapper {
                attrs,
                qualified_wrapped_trait,
            },
        ) in &self.trait_wrappers
        {
            let common_attrs = self.common_attrs(qualified_trait_wrapper);
            let attrs = join_attrs(attrs.iter().chain(&common_attrs));
            let (_, trait_wrapper) = qualified_trait_wrapper.extract_initial_path();
            writeln!(
                file,
                "{attrs}pub trait {}: {} {{",
                trait_wrapper.to_string(),
                qualified_wrapped_trait.to_string()
            )?;
            if let Some(fns) = self.fns.remove(qualified_trait_wrapper) {
                for Fn {
                    docs,
                    attrs,
                    sig,
                    body,
                } in fns
                {
                    let docs = docify(&docs.unwrap_or_default());
                    let attrs = join_attrs(&remove_common_attrs(attrs, &common_attrs));
                    writeln!(file, "{docs}{attrs}{sig} {{{body}}}")?;
                }
            }
            writeln!(
                file,
                "}}

{attrs}impl<T> {} for T where T: {} {{}}",
                trait_wrapper.to_string(),
                qualified_wrapped_trait.to_string()
            )?;
        }
        Ok(())
    }

    /// Writes the wrapper struct trait definition followed by the implementation of that trait for
    /// the wrapped struct.
    fn write_struct_wrappers(&mut self, file: &mut File) -> Result<()> {
        for (
            qualified_struct_wrapper,
            StructWrapper {
                attrs,
                qualified_wrapped_struct,
                must_be_sized,
                must_be_write,
            },
        ) in &self.struct_wrappers
        {
            let common_attrs = self.common_attrs(qualified_struct_wrapper);
            let attrs = join_attrs(attrs.iter().chain(&common_attrs));
            let (_, struct_wrapper) = qualified_struct_wrapper.extract_initial_path();
            let mut supertraits = String::new();
            if *must_be_sized {
                push_supertrait(&mut supertraits, "Sized");
            }
            if *must_be_write {
                push_supertrait(&mut supertraits, "std::io::Write");
            }
            let supertraits = if supertraits.is_empty() {
                String::new()
            } else {
                format!(": {supertraits}")
            };
            writeln!(
                file,
                "{attrs}pub trait {}{supertraits} {{",
                struct_wrapper.to_string(),
            )?;
            if let Some(fns) = self.fns.get(qualified_struct_wrapper) {
                for Fn {
                    docs,
                    attrs,
                    sig,
                    body: _,
                } in fns
                {
                    let docs = docify(&docs.clone().unwrap_or_default());
                    let attrs = join_attrs(&remove_common_attrs(attrs.clone(), &common_attrs));
                    writeln!(file, "{docs}{attrs}{sig};")?;
                }
            }
            writeln!(
                file,
                "\
}}
{attrs}impl {} for {} {{",
                struct_wrapper.to_string(),
                qualified_wrapped_struct.to_string()
            )?;
            if let Some(fns) = self.fns.remove(qualified_struct_wrapper) {
                for Fn {
                    docs: _,
                    attrs,
                    sig,
                    body,
                } in fns
                {
                    // smoelius: The docs were printed as part of the struct wrapper definition
                    // above. Don't print the docs again.
                    let attrs = join_attrs(&remove_common_attrs(attrs, &common_attrs));
                    writeln!(file, "{attrs}{sig} {{{body}}}")?;
                }
            }
            writeln!(file, "}}")?;
        }
        Ok(())
    }

    fn write_fns(&self, file: &mut File) -> Result<()> {
        for (qualified_struct_wrapper, fns) in &self.fns {
            assert!(qualified_struct_wrapper.is_empty());
            writeln!(file)?;
            for Fn {
                docs,
                attrs,
                sig,
                body,
            } in fns
            {
                let docs = docify(&docs.clone().unwrap_or_default());
                let attrs = join_attrs(attrs);
                writeln!(file, "{docs}{attrs}{sig} {{{body}}}")?;
            }
        }
        Ok(())
    }

    fn common_attrs(&self, qualified_type_wrapper: &[Token]) -> Vec<String> {
        let Some(fns) = self.fns.get(qualified_type_wrapper) else {
            return Vec::new();
        };
        let mut iter = fns.iter();
        let mut attrs = iter.next().unwrap().attrs.clone();
        for fn_ in iter {
            attrs.retain(|x| fn_.attrs.iter().any(|y| x == y));
        }
        attrs
    }
}

#[rustfmt::skip]
const HEADER: &str = "\
// This file was automatically generated by `elaborate`.
// https://github.com/trailofbits/elaborate

#[allow(unused_imports)]
use anyhow::Context;";

fn write_header(file: &mut File) -> Result<()> {
    writeln!(file, "{HEADER}")?;
    Ok(())
}

fn docify(docs: &str) -> String {
    docs.lines().map(|line| format!("/// {line}\n")).collect()
}

fn join_attrs<'a>(attrs: impl IntoIterator<Item = &'a String>) -> String {
    let mut attrs = attrs.into_iter().cloned().collect::<Vec<_>>();
    attrs.sort();
    attrs.dedup();
    attrs.join("")
}

fn remove_common_attrs(attrs: Vec<String>, common_attrs: &[String]) -> Vec<String> {
    attrs
        .into_iter()
        .filter(|attr| !common_attrs.contains(attr))
        .collect()
}

fn push_supertrait(supertraits: &mut String, supertrait: &str) {
    if !supertraits.is_empty() {
        *supertraits += " + ";
    }
    *supertraits += supertrait;
}
