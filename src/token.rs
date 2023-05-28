use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Illegal(u8),
    EOF,

    // identifiers + literals
    Ident(String),
    Int(String),

    // operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    Lt,
    Gt,

    Eq,
    NotEq,

    // delimiters
    Comma,
    Semicolon,

    LParen,
    RParen,
    LBrace,
    RBrace,

    // keywords
    Function,
    Let,
    If,
    Else,
    Return,
    True,
    False,
}

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, Token> = {
        use Token::*;

        let mut keywords = HashMap::new();
        keywords.insert("let", Let);
        keywords.insert("fn", Function);
        keywords.insert("if", If);
        keywords.insert("else", Else);
        keywords.insert("return", Return);
        keywords.insert("true", True);
        keywords.insert("false", False);
        keywords
    };
}

pub fn lookup_ident(s: String) -> Token {
    let kind = KEYWORDS.get(s.as_str());

    if let Some(kind) = kind {
        kind.clone()
    } else {
        Token::Ident(s)
    }
}
