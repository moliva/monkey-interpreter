use crate::token::{lookup_ident, Token};

pub struct Lexer {
    input: Vec<u8>,
    /// current position to input (points to curent char)
    position: usize,
    /// current reading position in input (after current char)
    read_position: usize,
    /// current char under examination
    current: u8,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut lexer = Self {
            input: input.to_owned().into_bytes(),
            position: Default::default(),
            read_position: Default::default(),
            current: Default::default(),
        };
        lexer.read_char();

        lexer
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.current = b'\0';
        } else {
            self.current = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&self) -> u8 {
        if self.read_position >= self.input.len() {
            b'\0'
        } else {
            self.input[self.read_position]
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        use Token::*;
        let token = match self.current {
            b'=' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Eq
                } else {
                    Assign
                }
            }
            b'+' => Plus,
            b'-' => Minus,
            b'!' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    NotEq
                } else {
                    Bang
                }
            }
            b'/' => Slash,
            b'*' => Asterisk,
            b'<' => Lt,
            b'>' => Gt,
            b';' => Semicolon,
            b'(' => LParen,
            b')' => RParen,
            b',' => Comma,
            b'{' => LBrace,
            b'}' => RBrace,
            b'\0' => EOF,
            c => {
                if is_letter(c) {
                    let s = self.read_identifier();
                    return lookup_ident(s);
                } else if is_digit(c) {
                    let n = self.read_number();
                    return Int(n);
                } else {
                    Illegal(c)
                }
            }
        };

        self.read_char();

        token
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;

        while is_letter(self.current) {
            self.read_char();
        }

        let slice = &self.input[position..self.position];
        std::str::from_utf8(slice).unwrap().to_owned()
    }

    fn skip_whitespace(&mut self) {
        while self.current == b' '
            || self.current == b'\t'
            || self.current == b'\n'
            || self.current == b'\r'
        {
            self.read_char();
        }
    }

    fn read_number(&mut self) -> String {
        let position = self.position;

        while is_digit(self.current) {
            self.read_char();
        }

        let slice = &self.input[position..self.position];
        std::str::from_utf8(slice).unwrap().to_owned()
    }
}

fn is_digit(c: u8) -> bool {
    c.is_ascii_digit()
}

fn is_letter(ch: u8) -> bool {
    ch.is_ascii_lowercase() || ch.is_ascii_uppercase() || ch == b'_'
}

#[cfg(test)]
mod test {

    use super::Lexer;
    use crate::token::Token::*;

    #[test]
    fn tokenizes() {
        let input = "=+(){},;";

        let tokens = vec![
            Assign, Plus, LParen, RParen, LBrace, RBrace, Comma, Semicolon, EOF,
        ];

        let mut lexer = Lexer::new(input);

        for token in tokens {
            let next = lexer.next_token();

            assert_eq!(token, next);
        }
    }

    #[test]
    fn test_next_token() {
        let input = r#"let five = 5;
let ten = 10;

let add = fn(x, y) {
x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10;
10 != 9;
"#;

        let tokens = vec![
            Let,                        //{token.LET, "let"},
            Ident("five".to_owned()),   //{token.IDENT, "five"},
            Assign,                     //{token.ASSIGN, "="},
            Int("5".to_owned()),        //{token.INT, "5"},
            Semicolon,                  //{token.SEMICOLON, ";"},
            Let,                        //{token.LET, "let"},
            Ident("ten".to_owned()),    //{token.IDENT, "ten"},
            Assign,                     //{token.ASSIGN, "="},
            Int("10".to_owned()),       //{token.INT, "10"},
            Semicolon,                  //{token.SEMICOLON, ";"},
            Let,                        //{token.LET, "let"},
            Ident("add".to_owned()),    //{token.IDENT, "add"},
            Assign,                     //{token.ASSIGN, "="},
            Function,                   //{token.FUNCTION, "fn"},
            LParen,                     //{token.LPAREN, "("},
            Ident("x".to_owned()),      //{token.IDENT, "x"},
            Comma,                      //{token.COMMA, ","},
            Ident("y".to_owned()),      //{token.IDENT, "y"},
            RParen,                     //{token.RPAREN, ")"},
            LBrace,                     //{token.LBRACE, "{"},
            Ident("x".to_owned()),      //{token.IDENT, "x"},
            Plus,                       //{token.PLUS, "+"},
            Ident("y".to_owned()),      //{token.IDENT, "y"},
            Semicolon,                  //{token.SEMICOLON, ";"},
            RBrace,                     //{token.RBRACE, "}"},
            Semicolon,                  //{token.SEMICOLON, ";"},
            Let,                        //{token.LET, "let"},
            Ident("result".to_owned()), //{token.IDENT, "result"},
            Assign,                     //{token.ASSIGN, "="},
            Ident("add".to_owned()),    //{token.IDENT, "add"},
            LParen,                     //{token.LPAREN, "("},
            Ident("five".to_owned()),   //{token.IDENT, "five"},
            Comma,                      //{token.COMMA, ","},
            Ident("ten".to_owned()),    //{token.IDENT, "ten"},
            RParen,                     //{token.RPAREN, ")"},
            Semicolon,                  //{token.SEMICOLON, ";"},
            Bang,                       //
            Minus,                      //
            Slash,                      //
            Asterisk,                   //
            Int("5".to_owned()),        //
            Semicolon,                  //
            Int("5".to_owned()),        //
            Lt,                         //
            Int("10".to_owned()),       //
            Gt,                         //
            Int("5".to_owned()),        //
            Semicolon,                  //
            If,                         //
            LParen,                     //
            Int("5".to_owned()),        //
            Lt,                         //
            Int("10".to_owned()),       //
            RParen,                     //
            LBrace,                     //
            Return,                     //
            True,                       //
            Semicolon,                  //
            RBrace,                     //
            Else,                       //
            LBrace,                     //
            Return,                     //
            False,                      //
            Semicolon,                  //
            RBrace,                     //
            Int("10".to_owned()),       //
            Eq,                         //
            Int("10".to_owned()),       //
            Semicolon,                  //
            Int("10".to_owned()),       //
            NotEq,                      //
            Int("9".to_owned()),        //
            Semicolon,                  //
            EOF,                        //{token.EOF, ""},
        ];

        let mut lexer = Lexer::new(input);

        for token in tokens {
            let next = lexer.next_token();

            assert_eq!(token, next);
        }
    }
}
