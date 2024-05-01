use std::env;
use std::fmt::Display;
use std::fmt;
use std::fs;
use std::fs::File;
use std::os::unix::process::CommandExt;
use std::path::Path;
use std::process::Child;
use std::process::Command;
use std::thread::sleep_ms;
use std::vec::IntoIter;

#[derive(PartialEq, Debug)]
enum Token {
    OpenCurlyBrace,
    ClosedCurlyBrace,
    OpenParen,
    ClosedParen,
    Semicolon,
    IntegerKeyword,
    ReturnKeyword,
    EOF,
    Identifier(String),
    IntegerLiteral(String),
    Negation,
    BitwiseComplement,
    LogicalNegation
}

#[derive(Debug)]
enum LexError {
    InvalidSymbolError(char)
}

impl Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LexError::InvalidSymbolError(c) => 
                write!(f, "invalid symbol: {}", c),
        }
    }

}

struct Lexer {
    input: Vec<u8>,
    position: usize,
}

impl Lexer {
    fn new(input: String) -> Self {
        Self {
            input: input.into_bytes(),
            position: 0,
        }
    }
    fn lex(&mut self) -> Result<Vec<Token>, LexError> {
        let mut token_list = Vec::new();
        let mut token = self.next_token()?;
        while token != Token::EOF {
            token_list.push(token); 
            token = self.next_token()?;
        };
        Ok(token_list)
    }

    fn read_word(&mut self) -> String {
        let start = self.position;
        while self.input[self.position].is_ascii_alphabetic() || self.input[self.position].is_ascii_digit() {
            self.position += 1; 
        }
        let s = String::from_utf8_lossy(&self.input[start..self.position]).to_string();
        self.position -= 1;
        s
    }

    fn read_number(&mut self) -> String {
        let start = self.position;
        while self.input[self.position].is_ascii_digit() {
            self.position += 1; 
        }
        let n = String::from_utf8_lossy(&self.input[start..self.position]).to_string();
        self.position -= 1;
        n
    }

    fn skip_whitespace(&mut self) {
        let mut ch = self.input[self.position];
        while self.position < self.input.len() && ch.is_ascii_whitespace() {
            self.position += 1; 
            if self.position < self.input.len() {
                ch = self.input[self.position];
            } else {
                ch = 0;
            }
        }
    }

    fn next_token(&mut self) -> Result<Token, LexError> {
        if self.position > self.input.len() - 1{
            return Ok(Token::EOF);
        }
        self.skip_whitespace();
        // we do not talk about why this is getting called twice here
        // actually
        // just bc of whitespace at the end of the file
        // lol
        if self.position > self.input.len() - 1{
            return Ok(Token::EOF);
        }

        let tok = match self.input[self.position] {
            b'{' => Ok(Token::OpenCurlyBrace),
            b'}' => Ok(Token::ClosedCurlyBrace),
            b'(' => Ok(Token::OpenParen),
            b')' => Ok(Token::ClosedParen),
            b';' => Ok(Token::Semicolon),
            b'!' => Ok(Token::LogicalNegation),
            b'-' => Ok(Token::Negation),
            b'~' => Ok(Token::BitwiseComplement),
            b'a'..=b'z' => {
                let word = self.read_word();
                match word.as_str() {
                    "int" => Ok(Token::IntegerKeyword),
                    "return" => Ok(Token::ReturnKeyword),
                    _ => Ok(Token::Identifier(word)),
                }
            },
            b'0'..=b'9' => {
                let num = self.read_number();
                Ok(Token::IntegerLiteral(num))
            },
            x => Err(LexError::InvalidSymbolError(x as char))
        };

        self.position += 1;

        tok
    }
}

#[derive(Clone)]
struct Program {
    main_method: Box<FunctionDeclaration>
}

#[derive(Clone)]
struct FunctionDeclaration {
    name: String,
    body: Box<Statement>
}

#[derive(Clone)]
enum Statement {
    Return(Return)
}
#[derive(Clone)]
struct Return {
    body: Box<Expression>
}

#[derive(Clone)]
enum Expression {
    Constant(Constant),
    UnOp(UnOp)
}
#[derive(Clone)]
struct Constant {
    value: u32
}

#[derive(Clone)]
enum ParserError {
    GenericError(String),
    NumberTooBig
}

#[derive(Clone)]
struct UnOp {
    operator: Operator,
    exp: Box<Expression>
}

#[derive(Clone)]
enum Operator {
    Negation,
    BitwiseComplement,
    LogicalComplement
}

impl Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParserError::GenericError(s) => 
                write!(f, "parser error: {}", s),
            ParserError::NumberTooBig => 
                write!(f, "numbers provided should be max u32 size"),
        }
    }

}

struct Parser {
    tokens: IntoIter<Token>
} 

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: tokens.into_iter()
        }
    }

    fn parse(&mut self) -> Result<Program, ParserError> {
        let main_function = self.parse_function(true)?;

        Ok(Program {
            main_method: Box::new(main_function)
        })
    }

    fn get_operator_node(&mut self, tok: Token) -> Result<Operator, ParserError> {
        match tok {
            Token::Negation => Ok(Operator::Negation),
            Token::BitwiseComplement => Ok(Operator::BitwiseComplement),
            Token::LogicalNegation => Ok(Operator::LogicalComplement),
            _ => Err(ParserError::GenericError(String::from("unknown unary operator"))),
        }
    }

    fn parse_function(&mut self, is_main: bool) -> Result<FunctionDeclaration, ParserError> {
        let tok = self.tokens.next().ok_or(ParserError::GenericError(String::from("no integer keyword")))?;
        if tok != Token::IntegerKeyword {
            return Err(ParserError::GenericError(String::from("no int for function")));
        };
        let tok = self.tokens.next().ok_or(ParserError::GenericError(String::from("no ident")))?;
        let function_name = match tok {
            Token::Identifier(ident) => Ok(ident),
            _ => Err(ParserError::GenericError(String::from("not a valid function name"))),
        }?;

        if is_main && function_name != String::from("main") {
            return Err(ParserError::GenericError(String::from("no main function found")))
        };

        let tok = self.tokens.next().ok_or(ParserError::GenericError(String::from("no open paren")))?;
        if tok != Token::OpenParen {
            return Err(ParserError::GenericError(String::from("no open parenthensis")));
        };

        let tok = self.tokens.next().ok_or(ParserError::GenericError(String::from("no closed paren")))?;
        if tok != Token::ClosedParen {
            return Err(ParserError::GenericError(String::from("no closed parenthensis")));
        };

        let tok = self.tokens.next().ok_or(ParserError::GenericError(String::from("no open curly")))?;
        if tok != Token::OpenCurlyBrace {
            return Err(ParserError::GenericError(String::from("no open curly brace")));
        };

        let stmnt = self.parse_statement()?;

        let tok = self.tokens.next().ok_or(ParserError::GenericError(String::from("no closed curly")))?;
        if tok != Token::ClosedCurlyBrace {
            return Err(ParserError::GenericError(String::from("no closed curly brace")));
        };

        Ok(FunctionDeclaration {
            name: function_name,
            body: Box::new(stmnt)
        })
    }

    fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        let tok = self.tokens.next().ok_or(ParserError::GenericError(String::from("no tokens left")))?;
        if tok != Token::ReturnKeyword {
            return Err(ParserError::GenericError(String::from("no return in statement")));
        };
        let exp = self.parse_exp()?;
        let statement = Statement::Return(Return { body: Box::new(exp) });

        let tok = self.tokens.next().ok_or(ParserError::GenericError(String::from("too few tokens provided")))?;
        if tok != Token::Semicolon {
            return Err(ParserError::GenericError(String::from("no semicolon found in statement")));
        };

        return Ok(statement);
    }

    fn parse_exp(&mut self) -> Result<Expression, ParserError> {
        let tok = self.tokens.next().ok_or(ParserError::GenericError(String::from("too few tokens provided")))?;
        match tok {
            // const node
            Token::IntegerLiteral(n) => {
                let num = n.parse::<u32>().map_err(|_| {ParserError::NumberTooBig})?;
                Ok(Expression::Constant(Constant {value: num}))
            },
            // unary operator
            Token::Negation | Token::LogicalNegation | Token::BitwiseComplement => {
                let op = self.get_operator_node(tok)?;
                let inner_exp = self.parse_exp()?;
                return Ok(Expression::UnOp(UnOp { exp: Box::new(inner_exp), operator: op }))
            },
            _ => Err(ParserError::GenericError(String::from("expression must be of type integer"))),
        }
    }
}

struct Generator {
    ast: Program    
}

impl Generator {
    fn new(ast: Program) -> Self {
        Self {
            ast
        }
    }

    fn generate(&self) -> String {
        let mut s = self.generate_function(self.ast.clone().main_method);
        s.push(10 as char);
        s
    }

    fn generate_function(&self, func: Box<FunctionDeclaration>) -> String {
        let func = *func;
        let b = self.generate_statement(func.body);
        format!(".globl {}\n{}:\n{b}", func.name, func.name)
    }

    fn generate_statement(&self, stmnt: Box<Statement>) -> String {
        let stmnt = *stmnt;
        match stmnt {
            Statement::Return(Return { body: b }) => {
                let gb = self.generate_exp(b);
                format!("{gb}\nret") 
            }
        }
    }

    fn generate_exp(&self, exp: Box<Expression>) -> String {
        let exp = *exp;
        match exp {
            Expression::Constant(Constant { value: c }) => format!("movl ${}, %eax", c),
            Expression::UnOp(UnOp { operator, exp }) => {
                let inner = self.generate_exp(exp);
                match operator {
                    Operator::LogicalComplement => format!("{inner}\ncmpl $0, %eax\nmovl $0, %eax\nsete %al"),
                    Operator::BitwiseComplement => format!("{inner}\nnot %eax"),
                    Operator::Negation => format!("{inner}\nneg %eax"),
                }
            }
        }
    }
}

#[derive(Debug)]
enum CompileError {
    GenericError(String),
    IoError(String),
}

impl Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CompileError::IoError(p) => 
                write!(f, "file {} not found", p),
            Self::GenericError(m) =>
                write!(f, "{}", m),
        }
    }

}

struct Compiler {
    source_path: String
}

impl Compiler {
    fn new(source_path: String) -> Self {
        Self {
            source_path
        }
    }

    fn compile(&self) -> Result<String, CompileError> {
        let sp = Path::new(&self.source_path);
        let i = fs::read_to_string(sp).map_err(|_| CompileError::IoError(sp.to_string_lossy().to_string()))?;
        let mut l = Lexer::new(i);
        let tokens = l.lex().map_err(|e| CompileError::GenericError(e.to_string()))?;
        let mut p = Parser::new(tokens);
        let ast = p.parse().map_err(|e| CompileError::GenericError(e.to_string()))?;
        let g = Generator::new(ast);
        let asm = g.generate();
        Ok(asm)
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let path = args[1].to_owned();
    let file_stem = Path::new(&path).file_stem().unwrap().to_string_lossy();
    let pwd = Path::new(&path).parent().unwrap().to_string_lossy();

    let c = Compiler::new(path.clone());
    let asm = match c.compile() {
        Ok(asm) => asm,
        Err(e) => panic!("{}", e),
    };

    let binding = Path::new(&pwd.to_string()).join(Path::new(&file_stem.to_string()));
    let binary_path = binding.to_string_lossy();
    let asm_path = format!("{binary_path}.s");

    if let Err(e) = fs::write(&asm_path, asm) {
        panic!("{}", e);
    }

    match Command::new("gcc").arg("-m32").arg(&asm_path).arg("-o").arg(binary_path.to_string()).spawn() {
        Ok(mut c) => match c.wait() {
            Ok(_) => (), // println!("gcc finished with {}", es),
            Err(e) => panic!("{}", e),
        }
        Err(e) => panic!("{}", e),
    }
    match Command::new("rm").arg(&asm_path).spawn() {
        Ok(mut c) => match c.wait() {
            Ok(_) => (),//println!("removing assembly file finished with {}", es),
            Err(e) => panic!("{}", e),
        }
        Err(e) => panic!("{}", e),
    }
}
