use public_api::{PublicItem, tokens::Token};

pub trait PublicItemExt {
    fn printable_tokens(&self) -> Vec<Token>;
}

impl PublicItemExt for PublicItem {
    fn printable_tokens(&self) -> Vec<Token> {
        self.tokens()
            .filter(|token| !matches!(token, Token::Whitespace))
            .cloned()
            .collect::<Vec<_>>()
    }
}
