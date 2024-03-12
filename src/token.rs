use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum Token {
    Illegal(u8),
    EOF,

    // identifiers + literals
    Ident(String),
    Int(String),
    String(String),

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
    Colon,

    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    // keywords
    Function,
    Let,
    If,
    Else,
    Return,
    True,
    False,
}
impl Token {
    pub(crate) fn literal(&self) -> String {
        match self {
            Token::EOF => "\0",
            Token::Ident(l) => l,
            Token::Int(l) => l,
            Token::Assign => "=",
            Token::Plus => "+",
            Token::Minus => "-",
            Token::Bang => "!",
            Token::Asterisk => "*",
            Token::Slash => "/",
            Token::Lt => "<",
            Token::Gt => ">",
            Token::Eq => "==",
            Token::NotEq => "!=",
            Token::Comma => ",",
            Token::Semicolon => ";",
            Token::Colon => ":",
            Token::LParen => "(",
            Token::RParen => ")",
            Token::LBrace => "{",
            Token::RBrace => "}",
            Token::LBracket => "[",
            Token::RBracket => "]",
            Token::Function => "fn",
            Token::Let => "let",
            Token::If => "if",
            Token::Else => "else",
            Token::Return => "return",
            Token::True => "true",
            Token::False => "false",
            Token::String(s) => s,
            Token::Illegal(l) => return std::str::from_utf8(&[*l]).unwrap().to_owned(),
        }
        .to_owned()
    }
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
