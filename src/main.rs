#[derive(Clone, Debug, PartialEq)]
enum Token {
    ADD, SUB, 
    MUL, DIV, 
    NUM(i32), LPR, RPR
}

#[derive(Clone)]
struct TokenParser<'a> {
    input: &'a Vec<char>,
    idx: Option<usize>, 
}

impl<'a> TokenParser<'a> {
    fn new(input: &'a Vec<char>) -> Self {
        TokenParser {
            input,
            idx: Some(0),       
        }
    }

    fn lookahead_idx(&self) -> Option<usize> {
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

impl<'a> Iterator for TokenParser<'a> {
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
                    match self.lookahead_idx() {
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
        self.idx = self.lookahead_idx();
        return Some(token);
    }
}

trait Node {
    fn eval(&self) -> i32;
    fn repr(&self) -> String;
}

struct NumNode(i32);
struct NegNode(Box<dyn Node>);
struct ParNode(Box<dyn Node>);
struct MulNode(Box<dyn Node>, Box<dyn Node>);
struct DivNode(Box<dyn Node>, Box<dyn Node>);
struct AddNode(Box<dyn Node>, Box<dyn Node>);
struct SubNode(Box<dyn Node>, Box<dyn Node>);

impl Node for NumNode {
    fn eval(&self) -> i32 { self.0 }
    fn repr(&self) -> String { format!("{}", self.eval()) }
}
impl Node for NegNode {
    fn eval(&self) -> i32 { - self.0.eval() }
    fn repr(&self) -> String { format!("<-{}>", self.0.repr())}
}
impl Node for ParNode {
    fn eval(&self) -> i32 { self.0.eval() }
    fn repr(&self) -> String { format!("({})", self.0.repr())}
}
impl Node for MulNode {
    fn eval(&self) -> i32 { self.0.eval() * self.1.eval() }
    fn repr(&self) -> String { format!("<{}*{}>", self.0.repr(), self.1.repr())}
}
impl Node for DivNode {
    fn eval(&self) -> i32 { self.0.eval() / self.1.eval() }
    fn repr(&self) -> String { format!("<{}/{}>", self.0.repr(), self.1.repr())}
}
impl Node for AddNode {
    fn eval(&self) -> i32 { self.0.eval() + self.1.eval() }
    fn repr(&self) -> String { format!("<{}+{}>", self.0.repr(), self.1.repr())}
}
impl Node for SubNode {
    fn eval(&self) -> i32 { self.0.eval() - self.1.eval() }
    fn repr(&self) -> String { format!("<{}-{}>", self.0.repr(), self.1.repr())}
}

fn evaluate(p: &mut TokenParser) -> Box<dyn Node> {
    let (n, t) = parse_e(p);
    if t.is_some() {
        panic!("Extra tokens after expression.");
    }
    return n;
}

// <t1>+<t2>, <t1>-<t2>
fn parse_e(p: &mut TokenParser) -> (Box<dyn Node>, Option<Token>) {
    let (mut n0, t1) = parse_t(p);
    if t1.is_none(){
        return (n0, None);
    }

    let mut tv = t1.unwrap();
    while tv == Token::ADD || tv == Token::SUB {
        let (n1, tn) = parse_t(p);
        match tv {
            Token::ADD => {
                n0 = Box::new(AddNode(n0, n1));
            }, 
            Token::SUB => {
                n0 = Box::new(SubNode(n0, n1));
            }, 
            _ => panic!("Unreachable")
        }
        match tn {
            Some(next_token) => tv = next_token,
            None => return (n0, None),
        }
    };
    return (n0, Some(tv));
}

// <f1>*<f2>, <f1>/<f2>
fn parse_t(p: &mut TokenParser) -> (Box<dyn Node>, Option<Token>) {
    let (mut n0, t1) = parse_f(p);
    if t1.is_none() {
        return (n0, None);
    }
    let mut tv = t1.unwrap();
    while tv == Token::MUL || tv == Token::DIV {
        let (n1, tn) = parse_f(p);
        match tv {
            Token::MUL => {
                n0 = Box::new(MulNode(n0, n1));
            }, 
            Token::DIV => {
                n0 = Box::new(DivNode(n0, n1));
            }, 
            _ => panic!("Unreachable")
        }
        match tn {
            Some(next_token) => tv = next_token,
            None => return (n0, None),
        }
    };
    return (n0, Some(tv));
}

// num, -<num>, (<expr>)
fn parse_f(p: &mut TokenParser) -> (Box<dyn Node>, Option<Token>) {
    let t0 = p.next().unwrap_or_else(||panic!("empty"));
    match t0 {
        Token::NUM(num) => {
            return (Box::new(NumNode(num)), p.next());
        }
        Token::SUB => {
            let t1 = p.next().expect("Nothing follows neg!");
            if let Token::NUM(num) = t1 {
                return (Box::new(NegNode(Box::new(NumNode(num)))), p.next());
            }
            panic!("Non-num of follow neg!");
        }
        Token::LPR => {
            let (expr, t1) = parse_e(p);
            match t1 {
                Some(Token::RPR) => {
                    return (Box::new(ParNode(expr)), p.next());
                },
                _ => panic!("Open parenthesis."),
            }
        }
        _ => {
            panic!("Illegal factor.");
        }
    }
}


fn main(){
    let args = std::env::args().collect::<Vec<String>>();

    let n: Box<dyn Node>;

    if args.len() == 1 {
        println!("Calculator! Please input an expression:"); 
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).expect("Failed to read line");
        let input_vec = input.trim().to_string().chars().collect();
        let mut parser = TokenParser::new(&input_vec);
        n = evaluate(&mut parser);
    }
    else {
        let input = &args[1];
        let input_vec = input.trim().to_string().chars().collect();
        let mut parser = TokenParser::new(&input_vec);
        n = evaluate(&mut parser);
    }

    println!("REPR: {}", n.repr());
    println!("Result: {}", n.eval());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_expr1(){
        let input = "-1 * (-2 + 5)".chars().collect();
        let mut parser = TokenParser::new(&input);
        let n = evaluate(&mut parser);
        assert_eq!(n.eval(), -3);
    }

    #[test]
    fn test_expr2(){
        let input = "12 + 34 - (56 / 7) * 8".chars().collect();
        let mut parser = TokenParser::new(&input);
        let n = evaluate(&mut parser);
        assert_eq!(n.eval(), -18);
    }

}
