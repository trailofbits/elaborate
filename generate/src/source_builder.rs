use crate::{PublicItemMap, TokensExt};
use anyhow::Result;
use public_api::tokens::Token;
use std::{
    collections::{BTreeMap, BTreeSet},
    fs::{create_dir_all, File, OpenOptions},
    io::Write,
};

struct TraitDef {
    attrs: Vec<String>,
}

struct StructDefAndImpls {
    attrs: Vec<String>,
    def_and_impls: Vec<String>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct FnDef {
    attrs: Vec<String>,
    def: String,
}

pub struct Module<'map> {
    public_item_map: &'map PublicItemMap,
    submodules: BTreeMap<String, Module<'map>>,
    trait_defs: BTreeMap<Vec<Token>, TraitDef>,
    struct_def_and_impls: BTreeMap<Vec<Token>, StructDefAndImpls>,
    fn_defs: BTreeMap<Vec<Token>, BTreeSet<FnDef>>,
}

impl<'map> Module<'map> {
    pub fn new(public_item_map: &'map PublicItemMap) -> Self {
        Self {
            public_item_map,
            submodules: BTreeMap::default(),
            trait_defs: BTreeMap::default(),
            struct_def_and_impls: BTreeMap::default(),
            fn_defs: BTreeMap::default(),
        }
    }

    pub fn add_trait(&mut self, path: &[&str], attrs: &Vec<String>, tokens: &[Token]) {
        let module = self.module_by_path(path);
        module.trait_defs.insert(
            tokens.to_vec(),
            TraitDef {
                attrs: attrs.to_owned(),
            },
        );
    }

    pub fn add_struct_def(
        &mut self,
        path: &[&str],
        attrs: &Vec<String>,
        qualified_struct: &[Token],
        def_and_impls: &[String],
    ) {
        let module = self.module_by_path(path);
        module.struct_def_and_impls.insert(
            qualified_struct.to_vec(),
            StructDefAndImpls {
                attrs: attrs.to_owned(),
                def_and_impls: def_and_impls.to_vec(),
            },
        );
    }

    pub fn add_fn_def(
        &mut self,
        path: &[&str],
        qualified_trait_or_struct: &[Token],
        attrs: &Vec<String>,
        def: &str,
    ) {
        let module = self.module_by_path(path);
        module
            .fn_defs
            .entry(qualified_trait_or_struct.to_vec())
            .or_default()
            .insert(FnDef {
                attrs: attrs.to_owned(),
                def: def.to_owned(),
            });
    }

    fn module_by_path(&mut self, path: &[&str]) -> &mut Module<'map> {
        let public_item_map = self.public_item_map;
        let mut module = self;
        for &name in path {
            module = module
                .submodules
                .entry(name.to_owned())
                .or_insert_with(|| Module::new(public_item_map));
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
        self.write_traits(&mut file)?;
        writeln!(file)?;
        self.write_struct_def_and_impls(&mut file)?;
        writeln!(file)?;
        self.write_fn_defs(&mut file)?;

        for (name, submodule) in self.submodules {
            submodule.write(dir.join(name))?;
        }

        Ok(())
    }

    fn write_traits(&mut self, file: &mut File) -> Result<()> {
        for (qualified_trait, TraitDef { attrs }) in &self.trait_defs {
            let common_attrs = self.common_attrs(qualified_trait);
            let attrs = join_attrs(attrs.iter().chain(&common_attrs));
            let (_, trait_tokens) = qualified_trait.extract_initial_path();
            writeln!(
                file,
                "{attrs}pub trait {}: {} {{",
                trait_tokens.to_string(),
                qualified_trait.to_string()
            )?;
            if let Some(fn_defs) = self.fn_defs.remove(qualified_trait) {
                for FnDef { attrs, def } in fn_defs {
                    let attrs = remove_common_attrs(attrs, &common_attrs);
                    writeln!(file, "{}{def}", join_attrs(&attrs))?;
                }
            }
            writeln!(
                file,
                "}}

{attrs}impl<T> {} for T where T: {} {{}}",
                trait_tokens.to_string(),
                qualified_trait.to_string()
            )?;
        }
        Ok(())
    }

    fn write_struct_def_and_impls(&self, file: &mut File) -> Result<()> {
        for (
            qualified_struct,
            StructDefAndImpls {
                attrs,
                def_and_impls,
            },
        ) in &self.struct_def_and_impls
        {
            let common_attrs = self.common_attrs(qualified_struct);
            let attrs = join_attrs(attrs.iter().chain(&common_attrs));
            for def_or_impl in def_and_impls {
                if def_or_impl.is_empty() {
                    continue;
                }
                writeln!(file, "{attrs}{def_or_impl}")?;
            }
        }
        Ok(())
    }

    fn write_fn_defs(&self, file: &mut File) -> Result<()> {
        for (qualified_struct, fn_defs) in &self.fn_defs {
            writeln!(file)?;
            let common_attrs = if qualified_struct.is_empty() {
                Vec::new()
            } else {
                self.common_attrs(qualified_struct)
            };
            if !qualified_struct.is_empty() {
                let attrs = self
                    .struct_def_and_impls
                    .get(qualified_struct)
                    .map(|struct_def_and_impls| struct_def_and_impls.attrs.clone())
                    .unwrap_or_default();
                let (_, struct_tokens) = qualified_struct.extract_initial_path();
                let attrs = join_attrs(attrs.iter().chain(&common_attrs));
                writeln!(file, "{attrs}impl {} {{", struct_tokens.to_string(),)?;
            }
            for FnDef { attrs, def } in fn_defs {
                let attrs = remove_common_attrs(attrs.clone(), &common_attrs);
                writeln!(
                    file,
                    "{}{}{def}",
                    join_attrs(&attrs),
                    if true { "pub " } else { "" }
                )?;
            }
            if !qualified_struct.is_empty() {
                writeln!(file, "}}")?;
            }
        }
        Ok(())
    }

    fn common_attrs(&self, tokens: &[Token]) -> Vec<String> {
        let Some(fn_defs) = self.fn_defs.get(tokens) else {
            return Vec::new();
        };
        let mut iter = fn_defs.iter();
        let mut attrs = iter.next().unwrap().attrs.clone();
        for fn_def in iter {
            attrs.retain(|x| fn_def.attrs.iter().any(|y| x == y));
        }
        attrs
    }
}

#[rustfmt::skip]
const HEADER: &str = "\
// This file was automatically generated by `elaborate`.
// https://github.com/smoelius/elaborate

#[allow(unused_imports)]
use anyhow::Context;";

fn write_header(file: &mut File) -> Result<()> {
    writeln!(file, "{HEADER}")?;
    Ok(())
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
