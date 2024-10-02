//! An [`Id`] may correspond to multiple [`PublicItem`]s. However, `elaborate` really only needs a
//! [`PublicItem`]'s tokens. So [`PublicItemMap`] verifies that each [`PublicItem`]'s tokens are
//! unambiguous, and panics if they are not.
//!
//! To prevent such ambiguity, certain paths must be ignored. An example is:
//!
//! ```ignore
//! std::os::unix::prelude::FileExt
//! ```
//!
//! If this path were not ignored, there would be a [`PublicItemMap`] with an [`Id`] with both that
//! path and:
//!
//! ```ignore
//! std::os::unix::fs::FileExt
//! ```
//!
//! Note that a [`PublicItemMap`] is implicitly a map from [`Id`]s to tokens, not the other way
//! around. So while filtering [`PublicItem`]s or rewriting their tokens may eliminate ambiguity
//! among an [`Id`]'s mapped-to tokens, it does not eliminate ambiguity among tokens' mapped-to
//! [`Id`]s, because the latter does not exist as a concept in [`PublicItemMap`].

#[expect(unused_imports)]
use public_api::PublicItem;

use crate::util::{PublicItemExt, TokensExt};
use anyhow::Result;
use once_cell::sync::Lazy;
use public_api::{tokens::Token, PublicApi};
use rustdoc_types::Id;
use std::{
    collections::{BTreeMap, HashMap, HashSet},
    io::Write,
};

use crate::std_other::{
    fs::{create_dir_all, OpenOptions},
    path::Path,
};

type Inner = HashMap<Id, HashSet<(Option<Id>, Vec<Token>)>>;

#[derive(Default)]
pub struct PublicItemMap {
    inner: Inner,
}

impl PublicItemMap {
    #[allow(clippy::type_complexity)]
    pub fn iter(&self) -> impl Iterator<Item = (&Id, &HashSet<(Option<Id>, Vec<Token>)>)> {
        self.inner.iter()
    }

    pub fn parent_id<'this>(&'this self, id: &Id) -> Option<&'this Id> {
        self.parent_id_inner(id, true)
    }

    /// Like [`Self::parent_id`], but does not check for ambiguity among the parents' tokens
    pub fn parent_id_unchecked<'this>(&'this self, id: &Id) -> Option<&'this Id> {
        self.parent_id_inner(id, false)
    }

    fn parent_id_inner<'this>(
        &'this self,
        id: &Id,
        check_for_ambiguity: bool,
    ) -> Option<&'this Id> {
        let public_items = self.inner.get(id).unwrap();
        let parent_ids = public_items
            .iter()
            .flat_map(|(parent_id, _)| parent_id)
            .collect::<HashSet<_>>();
        if parent_ids.is_empty() {
            return None;
        }
        if check_for_ambiguity {
            let parent_tokens = parent_ids
                .iter()
                .flat_map(|parent_id| {
                    // smoelius: A parent might not be in the map because it was ignored.
                    static EMPTY: Lazy<HashSet<(Option<Id>, Vec<Token>)>> = Lazy::new(HashSet::new);
                    let parent_public_items = self.inner.get(parent_id).unwrap_or(&EMPTY);
                    parent_public_items.iter().map(|(_, tokens)| tokens)
                })
                .collect::<HashSet<_>>();
            if parent_tokens.len() > 1 {
                let tokens = public_items
                    .iter()
                    .map(|(_, tokens)| tokens)
                    .collect::<HashSet<_>>();
                let parent_tokens = parent_ids
                    .iter()
                    .map(|parent_id| {
                        let parent_public_items = self.inner.get(parent_id).unwrap();
                        let parent_tokens = parent_public_items
                            .iter()
                            .map(|(_, tokens)| tokens)
                            .collect::<HashSet<_>>();
                        (parent_id, debug_tokens(parent_tokens))
                    })
                    .collect::<Vec<_>>();
                panic!(
                    "{id:?} has ambiguous parents
{id:?}: {:#?}
parents: {:#?}",
                    debug_tokens(tokens),
                    parent_tokens,
                );
            }
        }
        parent_ids.into_iter().next()
    }

    pub fn tokens<'this>(&'this self, id: &Id) -> &'this [Token] {
        let public_items = self.inner.get(id).unwrap();
        let tokens = public_items
            .iter()
            .map(|(_, tokens)| tokens)
            .collect::<HashSet<_>>();
        assert!(
            tokens.len() <= 1,
            "{id:?} has ambiguous tokens: {:#?}",
            debug_tokens(tokens),
        );
        tokens.into_iter().next().unwrap()
    }

    pub fn populate_from_public_api(
        &mut self,
        public_api: PublicApi,
        discard: impl Fn(&[Token]) -> Option<&str>,
    ) -> Result<()> {
        let mut discarded = BTreeMap::<String, Vec<Vec<Token>>>::new();
        for public_item in public_api.into_items() {
            let id = public_item.id();
            let tokens = public_item.printable_tokens();
            if let Some(reason) = discard(&tokens) {
                discarded.entry(reason.to_owned()).or_default().push(tokens);
                continue;
            }
            self.inner
                .entry(id.clone())
                .or_default()
                .insert((public_item.parent_id().cloned(), tokens));
        }
        write_discarded(&discarded)?;
        Ok(())
    }
}

fn write_discarded(discarded: &BTreeMap<String, Vec<Vec<Token>>>) -> Result<()> {
    #[cfg_attr(dylint_lib = "general", allow(abs_home_path))]
    let debug_output = Path::new(env!("CARGO_MANIFEST_DIR")).join("debug_output");

    create_dir_all(&debug_output).unwrap_or_default();

    for (reason, tokens) in discarded {
        let mut open_options = OpenOptions::new();
        open_options.create(true).truncate(true).write(true);
        let mut file = open_options.open(debug_output.join(reason).with_extension("txt"))?;

        for tokens in tokens {
            writeln!(file, "{tokens:?}")?;
        }
    }

    Ok(())
}

fn debug_tokens(iter: impl IntoIterator<Item = impl AsRef<[Token]>>) -> Vec<String> {
    iter.into_iter()
        .map(|tokens| tokens.as_ref().to_string())
        .collect()
}
