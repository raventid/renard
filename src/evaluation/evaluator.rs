use crate::ast;
use crate::evaluation::object;
use crate::evaluation::object::ObjectT;
use crate::lexer;
use crate::parser;
use crate::token;

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
    B(token::BlockStatement), // special case for block statement
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

// To save some typing in creating basic object
// we will use constants.

pub const NIL: object::Object = object::Object::Nil(object::Nil {});
pub const TRUE: object::Object = object::Object::Boolean(object::Boolean { value: true });
pub const FALSE: object::Object = object::Object::Boolean(object::Boolean { value: false });

pub fn eval(node: WN) -> object::Object {
    match node {
        WN::P(program) => eval_program(program),
        WN::S(statement) => match statement {
            token::Statements::ExpressionStatement(expr) => eval(WN::E(expr.expression)),
            token::Statements::LetStatement(_) => panic!("don't know how to handle let statement"),
            token::Statements::ReturnStatement(rs) => {
                let val = eval(WN::E(rs.return_value));
                // To see, why this early return is important look at the
                // test case `test_error_handling`.
                if is_error(&val) {
                    return val;
                }
                object::Object::ReturnValue(Box::new(object::ReturnValue { value: val }))
            }
        },
        WN::B(block) => eval_block_statement(block.statements),
        WN::E(expression) => match expression {
            token::Expression::IntegerLiteral(il) => {
                object::Object::Integer(object::Integer { value: il.value })
            }
            token::Expression::Identifier(_i) => panic!("don't how to handle identifier"),
            token::Expression::PrefixExpression(pe) => {
                let right = eval(WN::E(pe.right));
                if is_error(&right) {
                    return right;
                }
                match pe.operator.as_ref() {
                    "!" => match right {
                        TRUE => FALSE,
                        FALSE => TRUE,
                        NIL => TRUE,
                        _ => FALSE,
                    },
                    "-" => match right {
                        object::Object::Integer(int_obj) => {
                            object::Object::Integer(object::Integer {
                                value: -int_obj.value,
                            })
                        }
                        _ => new_error(format!("unknown operator: -{}", right.object_type()))
                    },
                    _ => new_error(format!("unknown operator: {} {}", pe.operator, right.object_type()))
                }
            }
            token::Expression::InfixExpression(ie) => {
                let left = eval(WN::E(ie.left));
                if is_error(&left) {
                    return left;
                }
                let right = eval(WN::E(ie.right));
                if is_error(&right) {
                    return right;
                }
                if let (object::Object::Integer(left_obj), object::Object::Integer(right_obj)) =
                    (left.clone(), right.clone())
                {
                    match ie.operator.as_ref() {
                        "+" => object::Object::Integer(object::Integer {
                            value: left_obj.value + right_obj.value,
                        }),
                        "-" => object::Object::Integer(object::Integer {
                            value: left_obj.value - right_obj.value,
                        }),
                        "*" => object::Object::Integer(object::Integer {
                            value: left_obj.value * right_obj.value,
                        }),
                        "/" => object::Object::Integer(object::Integer {
                            value: left_obj.value / right_obj.value,
                        }),
                        "<" => object::Object::Boolean(object::Boolean {
                            value: left_obj.value < right_obj.value
                        }),
                        ">" => object::Object::Boolean(object::Boolean {
                            value: left_obj.value > right_obj.value
                        }),
                        "==" => object::Object::Boolean(object::Boolean {
                            value: left_obj.value == right_obj.value
                        }),
                        "!=" => object::Object::Boolean(object::Boolean {
                            value: left_obj.value != right_obj.value
                        }),
                        _ => new_error(format!("unknown operator: INTEGER {} INTEGER", ie.operator))
                    }
                    // what if not boolean?
                    // TODO: maybe I should cover every option every time?
                } else if let (object::Object::Boolean(left_obj), object::Object::Boolean(right_obj)) =
                    (left.clone(), right.clone()) {
                        match ie.operator.as_ref() {
                            "==" => object::Object::Boolean(object::Boolean {
                                value: left_obj.value == right_obj.value,
                            }),
                            "!=" => object::Object::Boolean(object::Boolean {
                                value: left_obj.value != right_obj.value,
                            }),
                            _ => new_error(format!("unknown operator: BOOLEAN {} BOOLEAN", ie.operator))
                        }
                } else {
                        new_error(format!("type mismatch: {} {} {}", left.object_type(), ie.operator, right.object_type()))
                }
            }
            token::Expression::Boolean(b) => {
                // TODO: Check possible perf optimization? Needed?
                // Reuse TRUE and FALSE I mean
                object::Object::Boolean(object::Boolean { value: b.value })
            }
            token::Expression::IfExpression(ie) => {
                let condition = eval(WN::E(ie.condition));
                if is_error(&condition) {
                    return condition;
                }

                if is_truthy(condition) {
                    eval(WN::B(ie.consequence))
                } else {
                    match ie.alternative {
                        Some(alt) => eval(WN::B(alt)),
                        None => NIL,
                    }
                }
            },
            token::Expression::FunctionLiteral(_fl) => {
                panic!("don't how to handle function literal")
            }
            token::Expression::CallExpression(_ce) => panic!("don't how to handle call expression"),
        },
    }
}

// ************************************************
// ************************************************
// *********   HELPER FUNCTIONS   *****************
// ************************************************
// ************************************************

fn is_truthy(cond: object::Object) -> bool {
    match cond {
        NIL => false,
        TRUE => true,
        FALSE => false,
        _ => true,
    }
}

fn is_error(potential_error: &object::Object) -> bool {
    match potential_error {
        object::Object::Error(_) => true,
        _ => false,
    }
}

fn new_error(formated_string: String) -> object::Object {
    object::Object::Error(object::Error {
        message: formated_string,
    })
}

pub fn eval_program(program: ast::Program) -> object::Object {
    // TODO: wow, impressive, I see your skill
    let statements = program.statements;
    let mut statements = statements.into_iter();
    let mut size = statements.len();

    let result = loop {
        size = size - 1;
        let statement = match statements.next() {
            Some(statement) => statement,
            None => panic!("eval_statement is badly broken"),
        };

        let result = eval(WN::S(statement));

        // if statement is rendered into Return Value we have to
        // interupt the execution and return this value.
        match result.clone() {
            object::Object::ReturnValue(ret_val) => break ret_val.value,
            err @ object::Object::Error(_) => break err,
            otherwise => (),
        };

        if size == 0 {
            break result;
        };
    };

    return result;
}

pub fn eval_block_statement(statements: Vec<token::Statements>) -> object::Object {
    // TODO: wow, impressive, I see your skill
    let mut statements = statements.into_iter();
    let mut size = statements.len();

    let result = loop {
        size = size - 1;
        let statement = match statements.next() {
            Some(statement) => statement,
            None => panic!("eval_statement is badly broken"),
        };

        let result = eval(WN::S(statement));

        // if statement is rendered into Return Value we have to
        // interupt the execution and return this value.
        match result.clone() {
            // Do not unwrap return value. It will be unwraped at highest scope.
            val @ object::Object::ReturnValue(_) => break val,
            err @ object::Object::Error(_) => break err,
            otherwise => (),
        };

        if size == 0 {
            break result;
        };
    };

    return result;
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

    #[test]
    fn test_eval_boolean_expression() {
        let pairs = vec![
            ("true".to_string(), true),
            ("false".to_string(), false),
            ("1 < 2".to_string(), true),
            ("1 > 2".to_string(), false),
            ("1 < 1".to_string(), false),
            ("1 < 1".to_string(), false),
            ("1 == 1".to_string(), true),
            ("1 != 1".to_string(), false),
            ("1 == 2".to_string(), false),
            ("1 != 2".to_string(), true),
            ("true == true".to_string(), true),
            ("false == false".to_string(), true),
            ("true == false".to_string(), false),
        ];

        pairs.into_iter().for_each(|(value, expected)| {
            let evaluated = run_eval(value);
            assert_boolean_object(evaluated, expected);
        })
    }

    #[test]
    fn test_bang_operator() {
        let pairs = vec![
            ("!true".to_string(), false),
            ("!false".to_string(), true),
            ("!1".to_string(), false),
            ("!!true".to_string(), true),
            ("!!false".to_string(), false),
            ("!!1".to_string(), true),
        ];

        for (value, expected) in pairs {
            assert_boolean_object(run_eval(value), expected)
        }
    }

    #[test]
    fn test_prefix_negate_operator() {
        let pairs = vec![("-1".to_string(), -1), ("--1".to_string(), 1)];

        for (value, expected) in pairs {
            assert_integer_object(run_eval(value), expected)
        }
    }

    #[test]
    fn test_integer_arithmetic() {
        let pairs = vec![
            ("1 + 1 + 1".to_string(), 3),
            ("2 * 2 * 2".to_string(), 8),
            ("4 / 2 * 2".to_string(), 4),
            ("1 + 3 * 2".to_string(), 7),
        ];

        for (value, expected) in pairs {
            assert_integer_object(run_eval(value), expected)
        }
    }

    #[test]
    fn test_if_expression() {
        let pairs = vec![
            ("if (true) { 10 }".to_string(), Some(10)),
            ("if (false) { 10 }".to_string(), None),
            ("if (1) { 10 }".to_string(), Some(10)),
            ("if (1 < 2) { 10 }".to_string(), Some(10)),
            ("if (1 > 2) { 10 }".to_string(), None),
            ("if (1 < 2) { 10 } else { 20 }".to_string(), Some(10)),
            ("if (1 > 2) { 10 } else { 20 }".to_string(), Some(20)),
        ];

        for (expression, expected) in pairs {
            let evaluated = run_eval(expression);
            match expected {
                Some(val) => assert_integer_object(evaluated, val),
                None => assert_eq!(evaluated, evaluation::evaluator::NIL),
            }
        }
    }

    #[test]
    fn test_return_statement() {
        let pairs = vec![
            ("return 10;".to_string(), 10),
            ("return 10;".to_string(), 10),
            ("return 2 * 2; 69;".to_string(), 4),
            ("4; return 2 * 3; 8".to_string(), 6),
            (r###"
               if (2 > 1) {
                 if (2 > 1) {
                   return 999;
                 }

                 return 888;
               }
             "###.to_string(), 999)
        ];

        for (expression, expected) in pairs {
            let evaluated = run_eval(expression);
            assert_integer_object(evaluated, expected)
        }
    }

    #[test]
    fn test_error_handling() {
        let pairs = vec![
            ("2 + true;".to_string(), "type mismatch: INTEGER + BOOLEAN".to_string()),
            ("2 + true; 999".to_string(), "type mismatch: INTEGER + BOOLEAN".to_string()),
            ("-true;".to_string(), "unknown operator: -BOOLEAN".to_string()),
            ("false + true;".to_string(), "unknown operator: BOOLEAN + BOOLEAN".to_string()),
            (r###"
               if (2 > 1) {
                 if (2 > 1) {
                   return true + false;
                 }

                 return 888;
               }
             "###.to_string(), "unknown operator: BOOLEAN + BOOLEAN".to_string()),
            // This example checks if we stop evaling embedded path
            // and return first error.
            // In any compound AST component we have to stop evaluation
            // right after we encounter error.
            // In case we do not do this, we'll receive here an error like
            // `unknown operator: -BOOLEAN`, which is wrong.
            (r###"
               if (true + true) {
                 if (2 > 1) {
                   return -false;
                 }

                 return 888;
               }
             "###.to_string(), "unknown operator: BOOLEAN + BOOLEAN".to_string())
        ];

        for (expression, expected) in pairs {
            let evaluated = run_eval(expression);
            match evaluated {
                evaluation::object::Object::Error(err) => assert_eq!(err.message, expected),
                _ => panic!("expected error message, got {:?}", evaluated),
            }
        }
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

    fn assert_boolean_object(object: evaluation::object::Object, expected: bool) {
        let bool = match object {
            evaluation::object::Object::Boolean(b) => b,
            otherwise => panic!("expected boolean, got {:?}", otherwise),
        };

        assert_eq!(bool.value, expected)
    }
}
