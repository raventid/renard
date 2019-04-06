use crate::ast;
use crate::evaluation::environment;
use crate::evaluation::object;
use crate::evaluation::object::ObjectT;
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

pub fn eval(node: WN, env: &mut environment::Environment) -> object::Object {
    match node {
        WN::P(program) => eval_program(program, env),
        WN::S(statement) => match statement {
            token::Statements::ExpressionStatement(expr) => eval(WN::E(expr.expression), env),
            token::Statements::LetStatement(ls) => {
                let val = eval(WN::E(ls.value), env);
                if is_error(&val) {
                    return val;
                };
                env.set(ls.name.value, val).clone() // Hmmmmmmmmmmmm, change signature? To avoid cloning? Should work.
            }
            token::Statements::ReturnStatement(rs) => {
                let val = eval(WN::E(rs.return_value), env);
                // To see, why this early return is important look at the
                // test case `test_error_handling`.
                if is_error(&val) {
                    return val;
                }
                object::Object::ReturnValue(Box::new(object::ReturnValue { value: val }))
            }
        },
        WN::B(block) => eval_block_statement(block.statements, env),
        WN::E(expression) => match expression {
            token::Expression::IntegerLiteral(il) => {
                object::Object::Integer(object::Integer { value: il.value })
            }
            token::Expression::StringLiteral(sl) => {
                object::Object::Stringl(object::Stringl { value: sl.value })
            }
            token::Expression::ArrayLiteral(al) => {
                // first, eval arguments
                let elements = eval_expressions(al.elements, env);

                if elements.len() == 1 && is_error(&elements[0]) {
                    return elements[0].clone();
                }

                object::Object::Array(object::Array { elements })
            },
            token::Expression::IndexExpression(ie) => {
                let left = eval(WN::E(ie.left), env);
                if is_error(&left) {
                    return left;
                }
                let index = eval(WN::E(ie.index), env);
                if is_error(&index) {
                    return index;
                }
                if let (object::Object::Array(array), object::Object::Integer(i)) = (left.clone(), index) {
                    let elements = array.elements;
                    let i = i.value as usize;
                    match elements.get(i) {
                        Some(element) => element.clone(),
                        None => NIL,
                    }
                } else {
                    new_error(format!("index operator not supported: {}", left.object_type()))
                }
            },
            token::Expression::Identifier(i) => {
                match env.get(i.value.clone()) {
                    Some(value) => value.clone(), // Cloning one more time... Signature, sir?
                    None => match object::CoreFunc::try_new(i.value.clone()) {
                        Some(val) => val.clone(),
                        None => new_error(format!("identifier not found: {}", i.value)),
                    },
                }
            }
            token::Expression::PrefixExpression(pe) => {
                let right = eval(WN::E(pe.right), env);
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
                        _ => new_error(format!("unknown operator: -{}", right.object_type())),
                    },
                    _ => new_error(format!(
                        "unknown operator: {} {}",
                        pe.operator,
                        right.object_type()
                    )),
                }
            }
            token::Expression::InfixExpression(ie) => {
                let left = eval(WN::E(ie.left), env);
                if is_error(&left) {
                    return left;
                }
                let right = eval(WN::E(ie.right), env);
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
                            value: left_obj.value < right_obj.value,
                        }),
                        ">" => object::Object::Boolean(object::Boolean {
                            value: left_obj.value > right_obj.value,
                        }),
                        "==" => object::Object::Boolean(object::Boolean {
                            value: left_obj.value == right_obj.value,
                        }),
                        "!=" => object::Object::Boolean(object::Boolean {
                            value: left_obj.value != right_obj.value,
                        }),
                        _ => {
                            new_error(format!("unknown operator: INTEGER {} INTEGER", ie.operator))
                        }
                    }
                } else if let (
                    object::Object::Boolean(left_obj),
                    object::Object::Boolean(right_obj),
                ) = (left.clone(), right.clone())
                {
                    match ie.operator.as_ref() {
                        "==" => object::Object::Boolean(object::Boolean {
                            value: left_obj.value == right_obj.value,
                        }),
                        "!=" => object::Object::Boolean(object::Boolean {
                            value: left_obj.value != right_obj.value,
                        }),
                        _ => {
                            new_error(format!("unknown operator: BOOLEAN {} BOOLEAN", ie.operator))
                        }
                    }
                } else if let (
                    object::Object::Stringl(left_str),
                    object::Object::Stringl(right_str),
                ) = (left.clone(), right.clone())
                {
                    match ie.operator.as_ref() {
                        "+" => object::Object::Stringl(object::Stringl {
                            value: format!("{}{}", left_str.value, right_str.value),
                        }),
                        _ => new_error(format!("unknown operator: STRING {} STRING", ie.operator)),
                    }
                } else {
                    if left.same_tag(&right) {
                        new_error(format!(
                            "unknown operator: {} {} {}",
                            left.object_type(),
                            ie.operator,
                            right.object_type()
                        ))
                    } else {
                        new_error(format!(
                            "type mismatch: {} {} {}",
                            left.object_type(),
                            ie.operator,
                            right.object_type()
                        ))
                    }
                }
            }
            token::Expression::Boolean(b) => {
                // TODO: Check possible perf optimization? Needed?
                // Reuse TRUE and FALSE I mean
                object::Object::Boolean(object::Boolean { value: b.value })
            }
            token::Expression::IfExpression(ie) => {
                let condition = eval(WN::E(ie.condition), env);
                if is_error(&condition) {
                    return condition;
                }

                if is_truthy(condition) {
                    eval(WN::B(ie.consequence), env)
                } else {
                    match ie.alternative {
                        Some(alt) => eval(WN::B(alt), env),
                        None => NIL,
                    }
                }
            }
            token::Expression::FunctionLiteral(fl) => {
                let parameters = fl.parameters;
                let body = fl.body;
                // should I link to to this env or use copy of it
                object::Object::Function(object::Function {
                    parameters,
                    body,
                    env: env.clone(),
                })
            }
            token::Expression::CallExpression(ce) => {
                let fun = eval(WN::E(ce.function), env);
                if is_error(&fun) {
                    return fun;
                }

                // first, eval arguments
                let args = match ce.arguments {
                    Some(args) => eval_expressions(args, env),
                    None => eval_expressions(Vec::new(), env),
                };

                if args.len() == 1 && is_error(&args[0]) {
                    return args[0].clone();
                }

                apply_function(fun, args)
            }
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

pub fn new_error(formated_string: String) -> object::Object {
    object::Object::Error(object::Error {
        message: formated_string,
    })
}

pub fn eval_program(program: ast::Program, env: &mut environment::Environment) -> object::Object {
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

        let result = eval(WN::S(statement), env);

        // if statement is rendered into Return Value we have to
        // interupt the execution and return this value.
        match result.clone() {
            object::Object::ReturnValue(ret_val) => break ret_val.value,
            err @ object::Object::Error(_) => break err,
            _ => (),
        };

        if size == 0 {
            break result;
        };
    };

    return result;
}

pub fn eval_block_statement(
    statements: Vec<token::Statements>,
    env: &mut environment::Environment,
) -> object::Object {
    // TODO: wow, impressive, I see your skill
    // Maybe smth like this `iter.take_while(Result::is_ok).last().map(Result::unwrap)` ?
    let mut statements = statements.into_iter();
    let mut size = statements.len();

    let result = loop {
        size = size - 1;
        let statement = match statements.next() {
            Some(statement) => statement,
            None => panic!("eval_statement is badly broken"),
        };

        let result = eval(WN::S(statement), env);

        // if statement is rendered into Return Value we have to
        // interupt the execution and return this value.
        match result.clone() {
            // Do not unwrap return value. It will be unwraped at highest scope.
            val @ object::Object::ReturnValue(_) => break val,
            err @ object::Object::Error(_) => break err,
            _ => (),
        };

        if size == 0 {
            break result;
        };
    };

    return result;
}

fn eval_expressions(
    expressions: Vec<token::Expression>,
    env: &mut environment::Environment,
) -> Vec<object::Object> {
    // https://stackoverflow.com/questions/26368288/how-do-i-stop-iteration-and-return-an-error-when-iteratormap-returns-a-result
    let evaluated: Result<Vec<_>, _> = expressions
        .into_iter()
        .map(|expression| {
            let evaluated = eval(WN::E(expression), env);
            if is_error(&evaluated) {
                Err(evaluated)
            } else {
                Ok(evaluated)
            }
        })
        .collect();

    match evaluated {
        Ok(vals) => vals,
        Err(val) => vec![val],
    }
}

fn apply_function(fun: object::Object, args: Vec<object::Object>) -> object::Object {
    match fun {
        object::Object::Function(fun) => {
            let mut extended_env = extend_function_env(fun.clone(), args);
            let evaluated = eval(WN::B(fun.body), &mut extended_env);
            unwrap_return_value(evaluated)
        }
        object::Object::CoreFunc(fun) => fun.call(args),
        _ => new_error(format!("not a function: {}", fun.object_type())),
    }
}

fn extend_function_env(
    fun: object::Function,
    args: Vec<object::Object>,
) -> environment::Environment {
    let mut env = environment::Environment::new_enclosed_environment(fun.env);
    match fun.parameters {
        Some(params) => {
            for (param, arg) in params.into_iter().zip(args.into_iter()) {
                env.set(param.value, arg);
            }
            env
        }
        None => env,
    }
}

fn unwrap_return_value(obj: object::Object) -> object::Object {
    match obj {
        object::Object::ReturnValue(ret_val) => ret_val.value,
        _ => obj,
    }
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
    fn test_array_literal() {
        let evaluated = run_eval("[1, 1 + 1, 1 * 2]".to_string());
        match evaluated {
            evaluation::object::Object::Array(arr) => {
                assert_eq!(arr.elements.len(), 3);
                assert_integer_object(arr.elements[0].clone(), 1);
                assert_integer_object(arr.elements[1].clone(), 2);
                assert_integer_object(arr.elements[2].clone(), 2);
            }
            _ => panic!("expected array literal, got {:?}", evaluated),
        }
    }

    #[test]
    fn test_array_index_expression() {
        let pairs = vec![
            ("[1,2,3][0]".to_string(), Some(1)),
            ("[1,2,3][1]".to_string(), Some(2)),
            ("let i=0; [1,2,3][i];".to_string(), Some(1)),
            ("[1,2,3][3]".to_string(), None),
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
            (
                r###"
               if (2 > 1) {
                 if (2 > 1) {
                   return 999;
                 }

                 return 888;
               }
             "###
                .to_string(),
                999,
            ),
        ];

        for (expression, expected) in pairs {
            let evaluated = run_eval(expression);
            assert_integer_object(evaluated, expected)
        }
    }

    #[test]
    fn test_error_handling() {
        let pairs = vec![
            (
                "2 + true;".to_string(),
                "type mismatch: INTEGER + BOOLEAN".to_string(),
            ),
            (
                "2 + true; 999".to_string(),
                "type mismatch: INTEGER + BOOLEAN".to_string(),
            ),
            (
                "-true;".to_string(),
                "unknown operator: -BOOLEAN".to_string(),
            ),
            (
                "false + true;".to_string(),
                "unknown operator: BOOLEAN + BOOLEAN".to_string(),
            ),
            (
                r###"
               if (2 > 1) {
                 if (2 > 1) {
                   return true + false;
                 }

                 return 888;
               }
             "###
                .to_string(),
                "unknown operator: BOOLEAN + BOOLEAN".to_string(),
            ),
            // This example checks if we stop evaling embedded path
            // and return first error.
            // In any compound AST component we have to stop evaluation
            // right after we encounter error.
            // In case we do not do this, we'll receive here an error like
            // `unknown operator: -BOOLEAN`, which is wrong.
            (
                r###"
               if (true + true) {
                 if (2 > 1) {
                   return -false;
                 }

                 return 888;
               }
             "###
                .to_string(),
                "unknown operator: BOOLEAN + BOOLEAN".to_string(),
            ),
            // Check if we error on unknown identifier,
            // it's related to `test_let_statement()`
            (
                "unknown_bebe".to_string(),
                "identifier not found: unknown_bebe".to_string(),
            ),
            // String concatenation
            (
                r###" "Hey" - "Bebe" "###.to_string(),
                "unknown operator: STRING - STRING".to_string(),
            ),
        ];

        for (expression, expected) in pairs {
            let evaluated = run_eval(expression);
            match evaluated {
                evaluation::object::Object::Error(err) => assert_eq!(err.message, expected),
                _ => panic!("expected error message, got {:?}", evaluated),
            }
        }
    }

    #[test]
    fn test_let_statement() {
        let pairs = vec![
            ("let bebe = 1; bebe;".to_string(), 1),
            ("let bebe = 1 * 2; bebe;".to_string(), 2),
            ("let a = 1; let b = 2; let c = a + b; c;".to_string(), 3),
        ];

        for (expression, expected) in pairs {
            let evaluated = run_eval(expression);
            assert_integer_object(evaluated, expected);
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 1; }".to_string();

        let evaluated = run_eval(input);

        let fun = match evaluated {
            evaluation::object::Object::Function(fo) => fo,
            _ => panic!("expected function object, got {:?}", evaluated),
        };

        let params = match fun.parameters {
            Some(params) => params,
            None => panic!("expected to find params, but failed"),
        };

        assert_eq!(params.len(), 1);
        assert_eq!(params[0].to_string(), "x".to_string());
        assert_eq!(fun.body.to_string(), "(x + 1)".to_string());
    }

    #[test]
    fn test_function_application() {
        let pairs = vec![
            ("let id = fn(a) { a; }; id(1);".to_string(), 1),
            ("let id = fn(a) { return a; }; id(1);".to_string(), 1),
            ("let id = fn(a) { a; }(1);".to_string(), 1),
            // TODO: not a function called
            // TODO: wrong number of arguments passed
        ];

        for (expression, expected) in pairs {
            assert_integer_object(run_eval(expression), expected)
        }
    }

    #[test]
    fn test_string_concatenation() {
        let input = r###""Hey," + " " + "Bebe!""###.to_string();

        let evaluated = run_eval(input);
        match evaluated {
            evaluation::object::Object::Stringl(string) => {
                assert_eq!(string.value, "Hey, Bebe!".to_string())
            }
            _ => panic!("Expected string literal, got {:?}", evaluated),
        }
    }

    #[test]
    fn test_core_functions() {
        let pairs = vec![
            ("length(\"\")".to_string(), Ok(0)),
            ("length(\"bebe\")".to_string(), Ok(4)),
            (
                "length(1)".to_string(),
                Err("argument to `length` not supported, got INTEGER".to_string()),
            ),
            (
                "length(\"bebe\", \"milobe\")".to_string(),
                Err("wrong number of arguments: got=2, expected=1".to_string()),
            ),
            // additional test case for arrays
            ("length([1,2,3,4])".to_string(), Ok(4)),
            ("first([1,2,3,4])".to_string(), Ok(1)),
        ];

        for (expression, result) in pairs {
            match result {
                Ok(int) => assert_integer_object(run_eval(expression), int),
                Err(err) => match run_eval(expression) {
                    evaluation::object::Object::Error(evaluation::object::Error { message }) => {
                        assert_eq!(message, err)
                    }
                    _ => panic!("Expected error, got {:?}", err),
                },
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

        let mut env = evaluation::environment::Environment::new();

        evaluation::evaluator::eval(evaluation::evaluator::WN::P(program), &mut env)
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
