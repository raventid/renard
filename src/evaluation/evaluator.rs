use crate::evaluation::object;
use crate::lexer;
use crate::parser;
use crate::token;
use crate::ast;

// TODO: Consider this to be a hack.
//
// We have 3 types of untis now.
// 1. Program
// 2. Statement
// 3. Expression
//
// To generalize eval API we would wrap program in a special
// wrapper enum, which knows how to handle every unit.

#[derive(Debug)]
pub enum WrappedNode {
    P(ast::Program),
    S(token::Statements),
    E(token::Expression),
}
pub type WN = WrappedNode; // Just alias to avoid typing :)

// Option number 2.
// Write 3 different functions to work with 3
// different types of values.
//
// It's super bad design decision, cause I'll have to
// use different evalers in my REPL implementation,
// but I don't see the design clearly for now.

// For now we'll stick with design approach with WrappedNode.
// Perhaps use From/Into modification?

pub fn eval(node: WN) -> object::Object {
    match node {
        WN::P(program) => eval_statements(program.statements),
        WN::S(statement) => match statement {
            token::Statements::ExpressionStatement(expr) => eval(WN::E(expr.expression)),
            token::Statements::LetStatement(_) => panic!("don't know how to handle let statement"),
            token::Statements::ReturnStatement(_) => panic!("don't know how to handle return statement"),
        },
        WN::E(expression) => match expression {
            token::Expression::IntegerLiteral(il) => object::Object::Integer(object::Integer {
                value: il.value,
            }),
            token::Expression::Identifier(_i) => panic!("don't how to handle identifier"),
            token::Expression::PrefixExpression(_pe) => panic!("don't how to handle prefix expression"),
            token::Expression::InfixExpression(_ie) => panic!("don't how to handle infix expression"),
            token::Expression::Boolean(_b) => panic!("don't how to handle boolean"),
            token::Expression::IfExpression(_ie) => panic!("don't how to handle if expression"),
            token::Expression::FunctionLiteral(_fl) => panic!("don't how to handle function literal"),
            token::Expression::CallExpression(_ce) => panic!("don't how to handle call expression"),
        },
    }
}

pub fn eval_statements(statements: Vec<token::Statements>) -> object::Object {
    // TODO: not sure we need unwrap here.
    statements.into_iter().map(|statement| eval(WN::S(statement))).last().unwrap()
}

#[cfg(test)]
mod tests {
    use crate::evaluation;
    use crate::lexer;
    use crate::parser;
    use std::collections::HashMap;

    #[test]
    fn test_eval_integer_expression() {
        let pairs = vec![("1".to_string(), 1), ("2".to_string(), 2)];

        pairs.into_iter().for_each(|(value, expected)| {
            let evaluated = run_eval(value);
            assert_integer_object(evaluated, expected);
        })
    }

    fn run_eval(source_code: String) -> evaluation::object::Object {
        let lexer = lexer::Lexer::new(source_code);
        let mut parser = parser::Parser::new(lexer);

        let mut lambda_parsers = parser::LambdaParsers {
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };
        lambda_parsers.register_parsers();

        let program = parser.parse_program(&lambda_parsers);

        evaluation::evaluator::eval(evaluation::evaluator::WN::P(program))
    }

    fn assert_integer_object(object: evaluation::object::Object, expected: i32) {
        let integer = match object {
            evaluation::object::Object::Integer(int) => int,
            otherwise => panic!("expected integer, got {:?}", otherwise),
        };

        assert_eq!(integer.value, expected)
    }
}
