use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    // literals
    Number,
    String,
    Char,
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
    // special tokens
    Eof,
    Indent,
    Dedent,
    Newline,
    // error
    InvalidFloat,
    UnexpectedChar,
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
}

pub struct Tokenizer<'src> {
    source: &'src str,
    start: usize,
    end: usize,
    loc: Loc,
    /// Tracks the indentation level of the current line
    indent_stack: Vec<usize>,
    tokens: Vec<Token<'src>>,
}

impl<'src> Tokenizer<'src> {
    pub fn new(source: &'src str) -> Self {
        Self {
            source,
            start: 0,
            end: 0,
            loc: Loc { line: 1, col: 1 },
            indent_stack: vec![0],
            tokens: vec![],
        }
    }

    fn peek_char(&mut self) -> Option<char> {
        self.source.chars().nth(self.end)
    }

    fn handle_newline(&mut self) {
        self.simple_token(TokenKind::Newline);
        let mut indent = 0;
        while let Some(c) = self.peek_char() {
            if c == ' ' {
                indent += 1;
                self.next_char();
            } else {
                break;
            }
        }
        if indent > self.indent_stack.last().copied().unwrap() {
            self.indent_stack.push(indent);
            self.simple_token(TokenKind::Indent);
        } else {
            while let Some(&top) = self.indent_stack.last() {
                if indent < top {
                    self.indent_stack.pop();
                    self.simple_token(TokenKind::Dedent);
                } else {
                    break;
                }
            }
        }
    }

    fn next_char(&mut self) -> Option<char> {
        let c = self.source.chars().nth(self.end);
        self.end += 1;
        if let Some('\n') = c {
            self.loc.line += 1;
            self.loc.col = 1;
            self.handle_newline();
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

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek_char() {
            if c.is_whitespace() {
                self.next_char();
            } else {
                break;
            }
        }
    }

    fn read_number(&mut self){
        while let Some(c) = self.peek_char() {
            if c.is_digit(10) {
                self.next_char();
            } else {
                break;
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
                '0'..='9' => self.read_number(),
                ' ' => self.skip_whitespace(),
                '\n' => {
                    // next_char will handle the newline
                    self.next_char();
                }
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
    fn test_tokenize() {
        let src = "fn main() -> int:\n    return 0;";
        let mut tokenizer = Tokenizer::new(src);
        let tokens = tokenizer.tokenize();
        println!("{:?}", tokens);
    }
}
