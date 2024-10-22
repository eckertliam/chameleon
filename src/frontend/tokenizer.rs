use std::fmt::{self, Display};

// TODO: rewrite tokenizer to remove indent/dedent tokens and switch to a c-style syntax

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    // literals
    Number,
    String,
    Ident,
    True,
    False,
    // simple tokens
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,
    Lt,
    Lte,
    LtLt,
    Gt,
    Gte,
    GtGt,
    Eq,
    EqEq,
    Bang,
    BangEq,
    Amper,
    AmperAmper,
    Pipe,
    PipePipe,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Lbracket,
    Rbracket,
    Comma,
    Colon,
    Semicolon,
    Arrow,
    FatArrow,
    Dot,
    // keywords
    Fn,
    Let,
    Const,
    If,
    Else,
    Match,
    Return,   
    Private,
    Struct,
    Enum,
    Type,
    Trait,
    // special tokens
    Eof,
    // error
    UnexpectedChar,
}

impl TokenKind {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Number => "Number",
            Self::String => "String",
            Self::Ident => "Identifier",
            Self::True => "True",
            Self::False => "False",
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Star => "*",
            Self::Slash => "/",
            Self::Percent => "%",
            Self::Caret => "^",
            Self::Lt => "<",
            Self::Lte => "<=",
            Self::LtLt => "<<",
            Self::Gt => ">",
            Self::Gte => ">=",
            Self::GtGt => ">>",
            Self::Eq => "=",
            Self::EqEq => "==",
            Self::Bang => "!",
            Self::BangEq => "!=",
            Self::Amper => "&",
            Self::AmperAmper => "&&",
            Self::Pipe => "|",
            Self::PipePipe => "||",
            Self::Lparen => "(",
            Self::Rparen => ")",
            Self::Lbrace => "{{",
            Self::Rbrace => "}}",
            Self::Lbracket => "[",
            Self::Rbracket => "]",
            Self::Comma => ",",
            Self::Colon => ":",
            Self::Semicolon => ";",
            Self::Arrow => "->",
            Self::FatArrow => "=>",
            Self::Dot => ".",
            Self::Fn => "fn",
            Self::Let => "let",
            Self::Const => "const",
            Self::If => "if",
            Self::Else => "else",
            Self::Match => "match",
            Self::Return => "return",
            Self::Private => "private",
            Self::Struct => "struct",
            Self::Enum => "enum",
            Self::Type => "type",
            Self::Trait => "trait",
            Self::Eof => "EOF",
            Self::UnexpectedChar => "Error: Unexpected character",
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Loc {
    pub line: usize,
    pub col: usize,
}

impl Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

impl Default for Loc {
    fn default() -> Self {
        Self { line: 1, col: 1 }
    }
}

#[derive(Clone, Debug)]
pub struct Token<'src> {
    pub kind: TokenKind,
    pub lexeme: Option<&'src str>,
    pub loc: Loc,
}

impl<'src> Token<'src> {
    pub fn simple(kind: TokenKind, loc: Loc) -> Self {
        Self {
            kind,
            lexeme: None,
            loc,
        }
    }

    pub fn new(kind: TokenKind, lexeme: &'src str, loc: Loc) -> Self {
        Self {
            kind,
            lexeme: Some(lexeme),
            loc,
        }
    }

    /// returns either the lexeme or the tokenkind whatever will provide more information
    pub fn error_data(&self) -> String {
        self.lexeme.unwrap_or(self.kind.as_str()).to_string()
    }
}

pub struct Tokenizer<'src> {
    source: &'src str,
    start: usize,
    end: usize,
    loc: Loc,
    tokens: Vec<Token<'src>>,
}

impl<'src> Tokenizer<'src> {
    pub fn new(source: &'src str) -> Self {
        Self {
            source,
            start: 0,
            end: 0,
            loc: Loc { line: 1, col: 1 },
            tokens: vec![],
        }
    }

    fn peek_char(&mut self) -> Option<char> {
        self.source.chars().nth(self.end)
    }

    fn next_char(&mut self) -> Option<char> {
        let c = self.source.chars().nth(self.end);
        self.end += 1;
        if let Some('\n') = c {
            self.loc.line += 1;
            self.loc.col = 1;
        } else {
            self.loc.col += 1;
        }
        c
    }

    fn take_lexeme(&self) -> &'src str {
        &self.source[self.start..self.end]
    }

    fn token(&mut self, kind: TokenKind) {
        let lexeme = self.take_lexeme();
        self.tokens.push(Token::new(kind, lexeme, self.loc));
    }

    fn simple_token(&mut self, kind: TokenKind) {
        self.tokens.push(Token::simple(kind, self.loc));
    }

    /// advances the cursor by one character and adds a token
    fn next_simple(&mut self, kind: TokenKind) {
        self.next_char();
        self.simple_token(kind);
    }

    fn read_number(&mut self){
        while let Some(c) = self.peek_char() {
            if c.is_digit(10) {
                self.next_char();
            } else {
                break;
            }
        }
        if self.peek_char() == Some('.') {
            self.next_char();
            while let Some(c) = self.peek_char() {
                if c.is_digit(10) {
                    self.next_char();
                } else {
                    break;
                }
            }
        }
        self.token(TokenKind::Number);        
    }

    fn symbol_kind(&self) -> TokenKind {
        match self.take_lexeme() {
            "fn" => TokenKind::Fn,
            "let" => TokenKind::Let,
            "const" => TokenKind::Const,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "match" => TokenKind::Match,
            "return" => TokenKind::Return,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "private" => TokenKind::Private,
            "struct" => TokenKind::Struct,
            "enum" => TokenKind::Enum,
            "type" => TokenKind::Type,
            "trait" => TokenKind::Trait,
            _ => TokenKind::Ident,
        }
    }

    fn read_symbol(&mut self) {
        while let Some(c) = self.peek_char() {
            if c.is_alphanumeric() || c == '_' {
                self.next_char();
            } else {
                break;
            }
        }
        let kind = self.symbol_kind();
        if kind == TokenKind::Ident {
            self.token(kind);
        } else {
            self.simple_token(kind);
        }
    }

    pub fn tokenize(&mut self) -> &Vec<Token<'src>> {
        while let Some(c) = self.peek_char() {
            self.start = self.end;
            match c {
                c if c.is_whitespace() => {
                    self.next_char();
                }
                '0'..='9' => self.read_number(),
                '+' => self.next_simple(TokenKind::Plus),
                '-' => {
                    self.next_char();
                    if let Some('>') = self.peek_char() {
                        self.next_simple(TokenKind::Arrow);
                    } else {
                        self.simple_token(TokenKind::Minus);
                    }
                }
                '*' => self.next_simple(TokenKind::Star),
                '/' => self.next_simple(TokenKind::Slash),
                '%' => self.next_simple(TokenKind::Percent),
                '^' => self.next_simple(TokenKind::Caret),
                '<' => {
                    self.next_char();
                    if let Some('=') = self.peek_char() {
                        self.next_simple(TokenKind::Lte);
                    } else if let Some('<') = self.peek_char() {
                        self.next_simple(TokenKind::LtLt);
                    } else {
                        self.simple_token(TokenKind::Lt);
                    }
                }
                '>' => {
                    self.next_char();
                    if let Some('=') = self.peek_char() {
                        self.next_simple(TokenKind::Gte);
                    } else if let Some('>') = self.peek_char() {
                        self.next_simple(TokenKind::GtGt);
                    } else {
                        self.simple_token(TokenKind::Gt);
                    }
                }
                '=' => {
                    self.next_char();
                    if let Some('=') = self.peek_char() {
                        self.next_simple(TokenKind::EqEq);
                    } else if let Some('>') = self.peek_char() {
                        self.next_simple(TokenKind::FatArrow);
                    } else {
                        self.simple_token(TokenKind::Eq);
                    }
                }
                '!' => {
                    self.next_char();
                    if let Some('=') = self.peek_char() {
                        self.next_simple(TokenKind::BangEq);
                    } else {
                        self.simple_token(TokenKind::Bang);
                    }
                }
                '&' => {
                    self.next_char();
                    if let Some('&') = self.peek_char() {
                        self.next_simple(TokenKind::AmperAmper);
                    } else {
                        self.simple_token(TokenKind::Amper);
                    }
                }
                '|' => {
                    self.next_char();
                    if let Some('|') = self.peek_char() {
                        self.next_simple(TokenKind::PipePipe);
                    } else {
                        self.simple_token(TokenKind::Pipe);
                    }
                }
                '(' => self.next_simple(TokenKind::Lparen),
                ')' => self.next_simple(TokenKind::Rparen),
                '{' => self.next_simple(TokenKind::Lbrace),
                '}' => self.next_simple(TokenKind::Rbrace),
                '[' => self.next_simple(TokenKind::Lbracket),
                ']' => self.next_simple(TokenKind::Rbracket),
                ',' => self.next_simple(TokenKind::Comma),
                '.' => self.next_simple(TokenKind::Dot),
                ':' => self.next_simple(TokenKind::Colon),
                ';' => self.next_simple(TokenKind::Semicolon),
                'a'..='z' | 'A'..='Z' | '_' => self.read_symbol(),
                // TODO: handle string and char literals
                _ => {
                    self.token(TokenKind::UnexpectedChar);
                }
            };
        }
        self.tokens.push(Token::simple(TokenKind::Eof, self.loc));
        &self.tokens
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_tokens() {
        let input = "+-*/% ^ < <= << > >= >> = == ! != & && | || ( ) { } [ ] , : ; -> => .";
        let mut tokenizer = Tokenizer::new(input);
        let tokens = tokenizer.tokenize();
        
        let expected = vec![
            TokenKind::Plus, TokenKind::Minus, TokenKind::Star, TokenKind::Slash, TokenKind::Percent,
            TokenKind::Caret, TokenKind::Lt, TokenKind::Lte, TokenKind::LtLt, TokenKind::Gt, TokenKind::Gte,
            TokenKind::GtGt, TokenKind::Eq, TokenKind::EqEq, TokenKind::Bang, TokenKind::BangEq,
            TokenKind::Amper, TokenKind::AmperAmper, TokenKind::Pipe, TokenKind::PipePipe,
            TokenKind::Lparen, TokenKind::Rparen, TokenKind::Lbrace, TokenKind::Rbrace,
            TokenKind::Lbracket, TokenKind::Rbracket, TokenKind::Comma, TokenKind::Colon,
            TokenKind::Semicolon, TokenKind::Arrow, TokenKind::FatArrow, TokenKind::Dot,
            TokenKind::Eof
        ];

        assert_eq!(tokens.len(), expected.len());
        for (token, expected_kind) in tokens.iter().zip(expected.iter()) {
            assert_eq!(token.kind, *expected_kind);
        }
    }

    #[test]
    fn test_keywords() {
        let input = "fn let const if else match return private struct enum type trait";
        let mut tokenizer = Tokenizer::new(input);
        let tokens = tokenizer.tokenize();
        
        let expected = vec![
            TokenKind::Fn, TokenKind::Let, TokenKind::Const, TokenKind::If, TokenKind::Else,
            TokenKind::Match, TokenKind::Return, TokenKind::Private, TokenKind::Struct,
            TokenKind::Enum, TokenKind::Type, TokenKind::Trait, TokenKind::Eof
        ];

        assert_eq!(tokens.len(), expected.len());
        for (token, expected_kind) in tokens.iter().zip(expected.iter()) {
            assert_eq!(token.kind, *expected_kind);
        }
    }

    #[test]
    fn test_identifiers() {
        let inputs = ["variable_name", "camelCase", "PascalCase", "SCREAMING_SNAKE", "_underscore"];
        let acc = inputs.join(" ");
        let mut tokenizer = Tokenizer::new(&acc);
        let tokens = tokenizer.tokenize();
        assert_eq!(tokens.len(), inputs.len() + 1);
        for (token, expected) in tokens.iter().zip(inputs.iter()) {
            assert_eq!(token.kind, TokenKind::Ident);
            assert_eq!(token.lexeme.unwrap(), *expected);
        }
        assert_eq!(tokens.last().unwrap().kind, TokenKind::Eof);
    }

    #[test]
    fn test_numbers() {
        let inputs = ["123", "0", "42", "45.67", "0.1"];
        let acc = inputs.join(" ");
        let mut tokenizer = Tokenizer::new(&acc);
        let tokens = tokenizer.tokenize();
        assert_eq!(tokens.len(), inputs.len() + 1);
        for (token, expected) in tokens.iter().zip(inputs.iter()) {
            assert_eq!(token.kind, TokenKind::Number);
            assert_eq!(token.lexeme.unwrap(), *expected);
        }
        assert_eq!(tokens.last().unwrap().kind, TokenKind::Eof);
    }

    #[test]
    fn test_boolean_literals() {
        let input = "true false";
        let mut tokenizer = Tokenizer::new(input);
        let tokens = tokenizer.tokenize();
        
        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens[0].kind, TokenKind::True);
        assert_eq!(tokens[1].kind, TokenKind::False);
        assert_eq!(tokens[2].kind, TokenKind::Eof);
    }
}
