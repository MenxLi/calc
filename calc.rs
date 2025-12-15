
trait ASTNode {
    fn eval(&self) -> i32;
    fn repr(&self) -> String;
}

struct NumNode(i32);
struct NegNode(Box<dyn ASTNode>);
struct ParNode(Box<dyn ASTNode>);
struct MulNode(Box<dyn ASTNode>, Box<dyn ASTNode>);
struct DivNode(Box<dyn ASTNode>, Box<dyn ASTNode>);
struct AddNode(Box<dyn ASTNode>, Box<dyn ASTNode>);
struct SubNode(Box<dyn ASTNode>, Box<dyn ASTNode>);

impl ASTNode for NumNode {
    fn eval(&self) -> i32 { self.0 }
    fn repr(&self) -> String { format!("{}", self.eval()) }
}
impl ASTNode for NegNode {
    fn eval(&self) -> i32 { - self.0.eval() }
    fn repr(&self) -> String { format!("<-{}>", self.0.repr())}
}
impl ASTNode for ParNode {
    fn eval(&self) -> i32 { self.0.eval() }
    fn repr(&self) -> String { format!("({})", self.0.repr())}
}
impl ASTNode for MulNode {
    fn eval(&self) -> i32 { self.0.eval() * self.1.eval() }
    fn repr(&self) -> String { format!("<{}*{}>", self.0.repr(), self.1.repr())}
}
impl ASTNode for DivNode {
    fn eval(&self) -> i32 { self.0.eval() / self.1.eval() }
    fn repr(&self) -> String { format!("<{}/{}>", self.0.repr(), self.1.repr())}
}
impl ASTNode for AddNode {
    fn eval(&self) -> i32 { self.0.eval() + self.1.eval() }
    fn repr(&self) -> String { format!("<{}+{}>", self.0.repr(), self.1.repr())}
}
impl ASTNode for SubNode {
    fn eval(&self) -> i32 { self.0.eval() - self.1.eval() }
    fn repr(&self) -> String { format!("<{}-{}>", self.0.repr(), self.1.repr())}
}

#[derive(Clone, PartialEq, Debug)]
enum Token {
    ADD, SUB, 
    MUL, DIV, 
    NUM(i32), LPR, RPR
}

struct TokenParser {
    input: Vec<char>,
    idx: Option<usize>, 
}

impl TokenParser {
    fn new(input: String) -> Self {
        TokenParser {
            input: input.chars().collect(),
            idx: Some(0),       
        }
    }

    fn next_char_idx(&self) -> Option<usize> {
        let mut c : &char;
        let mut idx = self.idx?; 
        while idx + 1 < self.input.len() {
            idx += 1;
            c = &self.input[idx];
            if c.is_whitespace() {
                continue;
            }
            return Some(idx); 
        }
        return None;
    }
}

impl Iterator for TokenParser {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let c = &self.input[self.idx?];

        let token = match c {
            '+' => Token::ADD, 
            '-' => Token::SUB, 
            '*' => Token::MUL,
            '/' => Token::DIV, 
            '(' => Token::LPR, 
            ')' => Token::RPR, 
            '0'..='9' => {
                let mut accum = 0;
                loop {
                    let curr_c = self.input[self.idx.unwrap()];
                    let digit = curr_c.to_digit(10).unwrap() as i32;
                    accum = accum * 10 + digit;
                    match self.next_char_idx() {
                        Some(next_char_idx) => {
                            let next_char = &self.input[next_char_idx]; 
                            if next_char.is_digit(10) {
                                self.idx = Some(next_char_idx);
                            }
                            else {
                                break;
                            }
                        }, 
                        None => break,
                    }; 
                }
                Token::NUM(accum)
            }, 
            _ => panic!("Invalid token '{}' at index {}", c, self.idx.unwrap_or(0)),
        };
        self.idx = self.next_char_idx();
        return Some(token);
    }
}

// main entry point
fn evaluate(mut p: TokenParser) -> Box<dyn ASTNode> {
    let (n, t) = parse_e(&mut p);
    if t.is_some() {
        panic!("Error: Extra token after expression: {:?}", t.unwrap());
    }
    return n;
}

// <t1>+<t2>, <t1>-<t2>
fn parse_e(p: &mut TokenParser) -> (Box<dyn ASTNode>, Option<Token>) {
    let (mut n0, t1) = parse_t(p);
    let Some(mut tv) = t1 else { return (n0, None); };

    while tv == Token::ADD || tv == Token::SUB {
        let (n1, tn) = parse_t(p);
        n0 = match tv {
            Token::ADD => Box::new(AddNode(n0, n1)), 
            Token::SUB => Box::new(SubNode(n0, n1)), 
            _ => unreachable!()
        };
        match tn {
            Some(next_token) => tv = next_token,
            None => return (n0, None),
        }
    };
    return (n0, Some(tv));
}

// <f1>*<f2>, <f1>/<f2>
fn parse_t(p: &mut TokenParser) -> (Box<dyn ASTNode>, Option<Token>) {
    let (mut n0, t1) = parse_f(p);
    let Some(mut tv) = t1 else { return (n0, None); };

    while tv == Token::MUL || tv == Token::DIV {
        let (n1, tn) = parse_f(p);
        n0 = match tv {
            Token::MUL => Box::new(MulNode(n0, n1)), 
            Token::DIV => Box::new(DivNode(n0, n1)), 
            _ => unreachable!()
        }; 
        match tn {
            Some(next_token) => tv = next_token,
            None => return (n0, None),
        }
    };
    return (n0, Some(tv));
}

// num, -<num>, (<expr>)
fn parse_f(p: &mut TokenParser) -> (Box<dyn ASTNode>, Option<Token>) {
    let t0 = p.next().unwrap_or_else(||panic!("empty"));
    match t0 {
        Token::NUM(num) => {
            return (Box::new(NumNode(num)), p.next());
        }
        Token::SUB => {
            let t1 = p.next().expect("Error: Nothing follows NEG!");
            if let Token::NUM(num) = t1 {
                return (Box::new(NegNode(Box::new(NumNode(num)))), p.next());
            }
            panic!("Error: Non-num follows NEG: {:?}", t1);
        }
        Token::LPR => {
            let (expr, t1) = parse_e(p);
            match t1 {
                Some(Token::RPR) => {
                    return (Box::new(ParNode(expr)), p.next());
                },
                _ => panic!("Error: Open parenthesis."),
            }
        }
        _ => {
            panic!("Error: Illegal factor: {:?}", t0);
        }
    }
}


fn main(){
    let args = std::env::args().collect::<Vec<String>>();

    let n: Box<dyn ASTNode>;

    if args.len() == 1 {
        println!("Input your expr: "); 
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).expect("Failed to read line");
        let parser = TokenParser::new(input);
        n = evaluate(parser);
        println!("---")
    }
    else {
        let input = args[1].clone();
        let parser = TokenParser::new(input);
        n = evaluate(parser);
    }

    println!("REPR: {}", n.repr());
    println!("Result: {}", n.eval());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_expr1(){
        let parser = TokenParser::new(
            "-1 * (-2 + 5)".to_string()
        );
        let n = evaluate(parser);
        assert_eq!(n.eval(), -3);
    }

    #[test]
    fn test_expr2(){
        let parser = TokenParser::new(
            "12 + 34 - (56 / 7) * 8".to_string()
        );
        let n = evaluate(parser);
        assert_eq!(n.eval(), -18);
    }

    #[test]
    fn test_expr3(){
        let parser = TokenParser::new(
            "(-12 + 34) * ((56 / 7) + 8)".to_string()
        );
        let n = evaluate(parser);
        assert_eq!(n.eval(), 352);
    }
}
