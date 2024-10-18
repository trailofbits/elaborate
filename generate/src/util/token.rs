use public_api::tokens::Token;
use rustdoc_types::Type;

#[allow(dead_code)]
pub trait TokenExt {
    /// A symbol, like `=` or `::<`
    fn symbol(text: impl Into<String>) -> Self;
    /// A qualifier, like `pub` or `const`
    fn qualifier(text: impl Into<String>) -> Self;
    /// The kind of an item, like `function` or `trait`
    fn kind(text: impl Into<String>) -> Self;
    /// An identifier, like variable names or parts of the path of an item
    fn identifier(text: impl Into<String>) -> Self;
    /// The identifier self, the text can be `self` or `Self`
    fn self_(text: impl Into<String>) -> Self;
    /// The identifier for a function, like `fn_arg` in `comprehensive_api::functions::fn_arg`
    fn function(text: impl Into<String>) -> Self;
    /// A lifetime including the apostrophe `'`, like `'a`
    fn lifetime(text: impl Into<String>) -> Self;
    /// A keyword, like `impl`
    fn keyword(text: impl Into<String>) -> Self;
    /// A generic, like `T`
    fn generic(text: impl Into<String>) -> Self;
    /// A primitive type, like `usize`
    fn primitive(text: impl Into<String>) -> Self;
    /// A type, like `Iterator`
    fn type_(text: impl Into<String>) -> Self;
}

impl TokenExt for Token {
    /// A symbol, like `=` or `::<`
    fn symbol(text: impl Into<String>) -> Self {
        Self::Symbol(text.into())
    }
    /// A qualifier, like `pub` or `const`
    fn qualifier(text: impl Into<String>) -> Self {
        Self::Qualifier(text.into())
    }
    /// The kind of an item, like `function` or `trait`
    fn kind(text: impl Into<String>) -> Self {
        Self::Kind(text.into())
    }
    /// An identifier, like variable names or parts of the path of an item
    fn identifier(text: impl Into<String>) -> Self {
        Self::Identifier(text.into())
    }
    /// The identifier self, the text can be `self` or `Self`
    fn self_(text: impl Into<String>) -> Self {
        Self::Self_(text.into())
    }
    /// The identifier for a function, like `fn_arg` in `comprehensive_api::functions::fn_arg`
    fn function(text: impl Into<String>) -> Self {
        Self::Function(text.into())
    }
    /// A lifetime including the apostrophe `'`, like `'a`
    fn lifetime(text: impl Into<String>) -> Self {
        Self::Lifetime(text.into())
    }
    /// A keyword, like `impl`
    fn keyword(text: impl Into<String>) -> Self {
        Self::Keyword(text.into())
    }
    /// A generic, like `T`
    fn generic(text: impl Into<String>) -> Self {
        Self::Generic(text.into())
    }
    /// A primitive type, like `usize`
    fn primitive(text: impl Into<String>) -> Self {
        Self::Primitive(text.into())
    }
    /// A type, like `Iterator`
    fn type_(text: impl Into<String>) -> Self {
        Self::Type(text.into())
    }
}

macro_rules! ws {
    () => {
        Token::Whitespace
    };
}

// smoelius: `simplified_self` was originally copied from:
// https://github.com/cargo-public-api/cargo-public-api/blob/b6ec2e36010363206a223f5a97def697ebb558af/public-api/src/render.rs#L401-L428
pub fn simplified_self(name: &str, ty: &Type) -> Option<Vec<Token>> {
    if name == "self" {
        match ty {
            Type::Generic(name) if name == "Self" => Some(vec![Token::self_("self")]),
            Type::BorrowedRef {
                lifetime,
                is_mutable,
                type_,
            } => match type_.as_ref() {
                Type::Generic(name) if name == "Self" => {
                    let mut output = vec![Token::symbol("&")];
                    if let Some(lt) = lifetime {
                        output.extend([Token::lifetime(lt), ws!()]);
                    }
                    if *is_mutable {
                        output.extend([Token::keyword("mut"), ws!()]);
                    }
                    output.push(Token::self_("self"));
                    Some(output)
                }
                _ => None,
            },
            _ => None,
        }
    } else {
        None
    }
}
