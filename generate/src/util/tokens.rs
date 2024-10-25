use super::TokenExt;
use itertools::intersperse;
use once_cell::sync::Lazy;
use public_api::tokens::{tokens_to_string, Token};

static GATED_PATHS: Lazy<Vec<(Vec<Token>, &str)>> = Lazy::new(|| {
    const GATED_OS_PATHS: &[(&[&str], &str)] = &[
        (&["std", "os", "linux"], r#"target_os = "linux""#),
        (&["std", "os", "unix"], "unix"),
        (&["std", "os", "wasi"], r#"target_os = "wasi""#),
        (&["std", "os", "windows"], "windows"),
    ];

    std::iter::once((
        qualified_type_function(&["std", "io"], "BufRead", "skip_until"),
        r#"feature = "bufread_skip_until""#,
    ))
    .chain(std::iter::once((
        qualified_type(&["std", "io"], "RawOsError"),
        r#"feature = "raw_os_error_ty""#,
    )))
    .chain(std::iter::once((
        qualified_type_function(&["std", "thread"], "Builder", "spawn_unchecked"),
        r#"feature = "thread_spawn_unchecked""#,
    )))
    .chain(
        GATED_OS_PATHS
            .iter()
            .map(|&(prefix, gate)| (path_prefix(prefix), gate)),
    )
    .collect()
});

static COLON_SEALED: Lazy<Vec<Token>> =
    Lazy::new(|| vec![Token::symbol(":"), Token::type_("Sealed")]);

static OUTPUT_ADJUSTMENTS: Lazy<Vec<(Vec<Token>, &str)>> = Lazy::new(|| {
    vec![
        (
            vec![
                Token::symbol("&"),
                Token::keyword("mut"),
                Token::generic("Self"),
            ],
            "; self",
        ),
        (vec![Token::generic("Self")], ".into()"),
    ]
});

static UNCLONEABLE_TYPES: Lazy<Vec<Vec<Token>>> = Lazy::new(|| {
    vec![
        qualified_type(&["std", "io"], "Error"),
        qualified_type(&["std", "process"], "Child"),
        qualified_type(&["std", "thread"], "Builder"),
    ]
});

static UNSIZED_TYPES: Lazy<Vec<Vec<Token>>> = Lazy::new(|| {
    vec![
        qualified_type(&["std", "ffi"], "OsStr"),
        qualified_type(&["std", "path"], "Path"),
    ]
});

pub trait TokensExt {
    fn required_gates(&self) -> Vec<String>;

    fn strip_leading_qualifiers_and_fn(&self) -> &[Token];

    fn extract_trait(&self) -> &[Token];
    fn extract_struct(&self) -> &[Token];
    fn extract_initial_generic_params(&self) -> (&[Token], &[Token]);
    fn extract_initial_path(&self) -> (Vec<&str>, &[Token]);
    fn extract_initial_trait_or_type(&self) -> (&[Token], &[Token]);

    fn deanonymize_lifetimes(&self) -> (Vec<Token>, bool);
    fn remove_sealed(&self) -> Vec<Token>;
    fn selectively_collapse_self(&self, qualified_struct: &[Token]) -> Vec<Token>;
    fn error_type_is_self(&self) -> bool;
    fn rewrite_output_type(&self) -> Vec<Token>;
    fn turbofish(&self) -> Vec<Token>;

    fn has_typed_self(&self) -> Option<usize>;
    fn is_known_uncloneable_type(&self) -> bool;
    fn is_known_unsized_type(&self) -> bool;
    fn required_output_adjustment(&self) -> &str;
    fn output(&self) -> &[Token];
    fn output_offsets(&self) -> Option<(usize, usize)>;

    fn replace(&self, from: &[Token], to: &[Token]) -> (Vec<Token>, usize);
    fn position(&self, needle: &[Token]) -> Option<usize>;

    fn to_string(&self) -> String;

    /// WARNING: This method converts the tokens to a string without putting spaces between the
    /// tokens. The resulting code could be uncompileable.
    fn to_string_unchecked(&self) -> String;
}

impl TokensExt for [Token] {
    fn required_gates(&self) -> Vec<String> {
        GATED_PATHS
            .iter()
            .filter_map(|(needle, gate)| {
                if self.position(needle).is_some() {
                    Some(format!("#[cfg({gate})]\n"))
                } else {
                    None
                }
            })
            .collect()
    }

    fn strip_leading_qualifiers_and_fn(&self) -> &[Token] {
        let mut tokens = self;
        while !tokens.is_empty() && matches!(&tokens[0], Token::Qualifier(_)) {
            tokens = &tokens[1..];
        }
        assert!(matches!(&tokens[0], Token::Kind(s) if s == "fn"));
        &tokens[1..]
    }

    fn extract_trait(&self) -> &[Token] {
        if !self.is_empty() && matches!(&self[0], Token::Keyword(s) if s == "impl") {
            #[allow(clippy::manual_map)]
            if let Some(index) = self
                .iter()
                .position(|token| matches!(token, Token::Keyword(s) if s == "for"))
            {
                &self[1..index]
            } else {
                &[]
            }
        } else if self.len() >= 2
            && matches!(&self[0], Token::Qualifier(s) if s == "pub")
            && matches!(&self[1], Token::Kind(s) if s == "trait")
        {
            let start = 2;
            if let Some(offset) = self[start..]
                .iter()
                .position(|token| matches!(token, Token::Symbol(s) if s == ":"))
            {
                &self[start..start + offset]
            } else {
                &self[start..]
            }
        } else {
            &[]
        }
    }

    fn extract_struct(&self) -> &[Token] {
        if !self.is_empty() && matches!(&self[0], Token::Keyword(s) if s == "impl") {
            if let Some(index) = self
                .iter()
                .position(|token| matches!(token, Token::Keyword(s) if s == "for"))
            {
                &self[index + 1..]
            } else {
                &self[1..]
            }
        } else {
            &[]
        }
    }

    fn extract_initial_generic_params(&self) -> (&[Token], &[Token]) {
        if self.is_empty() || !matches!(&self[0], Token::Symbol(s) if s == "<") {
            return (&[], self);
        }
        let end = self
            .iter()
            .position(|token| matches!(token, Token::Symbol(s) if s == ">"))
            .unwrap();
        (&self[1..end], &self[end + 1..])
    }

    fn extract_initial_path(&self) -> (Vec<&str>, &[Token]) {
        let mut tokens = self;
        let mut path = Vec::new();
        while tokens.len() >= 2
            && matches!(tokens[0], Token::Identifier(_))
            && matches!(&tokens[1], Token::Symbol(s) if s == "::")
        {
            path.push(tokens[0].text());
            tokens = &tokens[2..];
        }
        // smoelius: Sanity.
        // smoelius: Why do I not need `Token::Trait` here also?
        // smoelius: Because that variant doesn't exist. Traits are handled by `Token::Type`.
        assert!(
            !tokens.is_empty() && matches!(tokens[0], Token::Type(_) | Token::Function(_)),
            "{tokens:?}"
        );
        (path, tokens)
    }

    fn extract_initial_trait_or_type(&self) -> (&[Token], &[Token]) {
        if matches!(self[0], Token::Function(_)) {
            return (&[], self);
        }
        let mut i = 0;
        while i + 1 < self.len() && !matches!(self[i + 1], Token::Function(_)) {
            i += 1;
        }
        assert!(matches!(&self[i], Token::Symbol(s) if s == "::"));
        assert!(matches!(self[i + 1], Token::Function(_)));
        (&self[..i], &self[i + 1..])
    }

    fn deanonymize_lifetimes(&self) -> (Vec<Token>, bool) {
        let (tokens, replacements) =
            self.replace(&[Token::lifetime("'_")], &[Token::lifetime("'a")]);

        (tokens, replacements != 0)
    }

    /// It might be preferable to remove the `Sealed` bound from the [`rustdoc_types::Trait`] before
    /// tokens are generated. However, that would require modifying the Rustdoc JSON before it is
    /// converted to [`public_api::PublicItem`]s.
    fn remove_sealed(&self) -> Vec<Token> {
        let (tokens, replacements) = self.replace(&COLON_SEALED, &[]);

        assert!(replacements != 0);

        tokens
    }

    fn selectively_collapse_self(&self, qualified_struct: &[Token]) -> Vec<Token> {
        let start = if let Some(index) = self.has_typed_self().map(|index| {
            // smoelius: `+ 2` for `self` and ':'.
            index + 2
        }) {
            index
        } else if let Some(index) = self.position(&[Token::symbol("->")]).map(|index| index + 1) {
            index
        } else {
            return self.to_vec();
        };

        let end = self
            .position(&[Token::keyword("where")])
            .unwrap_or(self.len());

        let (tokens, _) = self[start..end].replace(qualified_struct, &[Token::generic("Self")]);

        [&self[..start], &tokens, &self[end..]].concat()
    }

    fn error_type_is_self(&self) -> bool {
        static SUFFIX: Lazy<Vec<Token>> = Lazy::new(|| {
            vec![
                Token::symbol(","),
                Token::generic("Self"),
                Token::symbol(">"),
            ]
        });

        self.output().ends_with(&SUFFIX)
    }

    fn rewrite_output_type(&self) -> Vec<Token> {
        static PREFIX: Lazy<Vec<Token>> = Lazy::new(|| {
            vec![
                Token::keyword("crate"),
                Token::symbol("::"),
                Token::type_("rewrite_output_type"),
                Token::symbol("!"),
                Token::symbol("("),
            ]
        });
        static SUFFIX: Lazy<Vec<Token>> = Lazy::new(|| vec![Token::symbol(")")]);

        let (start, end) = self.output_offsets().unwrap();

        let tokens = &self[start..end];

        [&self[..start], &PREFIX, tokens, &SUFFIX, &self[end..]].concat()
    }

    fn turbofish(&self) -> Vec<Token> {
        let mut tokens = self;
        if tokens.len() < 2 {
            return tokens.to_vec();
        }
        let mut buf = Vec::new();
        loop {
            buf.push(tokens[0].clone());
            if matches!(tokens[0], Token::Type(_))
                && matches!(&tokens[1], Token::Symbol(s) if s == "<")
            {
                buf.push(Token::symbol("::"));
            }
            tokens = &tokens[1..];
            if tokens.len() < 2 {
                break;
            }
        }
        buf.push(tokens[0].clone());
        buf
    }

    fn has_typed_self(&self) -> Option<usize> {
        // smoelius: `Token::identifier("self")` as opposed to `Token::self_("self")` appears to be
        // a bug.
        self.position(&[Token::identifier("self"), Token::symbol(":")])
    }

    fn is_known_uncloneable_type(&self) -> bool {
        UNCLONEABLE_TYPES
            .iter()
            .any(|qualified_struct| self == qualified_struct)
    }

    fn is_known_unsized_type(&self) -> bool {
        UNSIZED_TYPES
            .iter()
            .any(|qualified_struct| self == qualified_struct)
    }

    fn required_output_adjustment(&self) -> &str {
        let Some(start) = self.position(&[Token::symbol("->")]).map(|i| i + 1) else {
            return "";
        };
        let end = self
            .position(&[Token::keyword("where")])
            .unwrap_or(self.len());
        for (output, adjustment) in &*OUTPUT_ADJUSTMENTS {
            if &self[start..end] == output {
                return adjustment;
            }
        }
        ""
    }

    fn output(&self) -> &[Token] {
        self.output_offsets()
            .map_or(&[], |(start, end)| &self[start..end])
    }

    fn output_offsets(&self) -> Option<(usize, usize)> {
        let start = self.position(&[Token::symbol("->")]).map(|i| i + 1)?;
        let end = self
            .position(&[Token::keyword("where")])
            .unwrap_or(self.len());
        Some((start, end))
    }

    fn replace(&self, from: &[Token], to: &[Token]) -> (Vec<Token>, usize) {
        let mut tokens = self.to_vec();
        let mut replacements = 0;
        while let Some(index) = tokens.position(from) {
            tokens = [&tokens[..index], to, &tokens[index + from.len()..]].concat();
            replacements += 1;
        }
        (tokens, replacements)
    }

    fn position(&self, needle: &[Token]) -> Option<usize> {
        self.windows(needle.len()).position(|w| w == needle)
    }

    fn to_string(&self) -> String {
        tokens_to_string(&intersperse(self.iter().cloned(), Token::Whitespace).collect::<Vec<_>>())
    }

    fn to_string_unchecked(&self) -> String {
        tokens_to_string(self)
    }
}

pub fn qualified_function(path: &[&str], name: &str) -> Vec<Token> {
    [path_prefix(path).as_slice(), &[Token::function(name)]].concat()
}

pub fn qualified_type(path: &[&str], ty: &str) -> Vec<Token> {
    [path_prefix(path).as_slice(), &[Token::type_(ty)]].concat()
}

pub fn qualified_type_function(path: &[&str], ty: &str, name: &str) -> Vec<Token> {
    [
        qualified_type(path, ty).as_slice(),
        &[Token::symbol("::"), Token::function(name)],
    ]
    .concat()
}

pub fn path_prefix(prefix: &[&str]) -> Vec<Token> {
    prefix
        .iter()
        .flat_map(|&s| [Token::identifier(s), Token::symbol("::")])
        .collect()
}
