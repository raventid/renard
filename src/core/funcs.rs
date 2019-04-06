use crate::evaluation::evaluator::new_error;
use crate::evaluation::object;
use crate::evaluation::object::ObjectT;
use lazy_static::lazy_static;
use std::collections::HashMap;

pub type FunctionName = String;
pub type Arity = u8;

// To register new function in the system we have to add it to
// two different places.
// First add it here, by registering its arity.
lazy_static! {
    pub static ref CORE_REGISTRY: HashMap<FunctionName, Arity> =
        [("length".to_string(), 1), ("first".to_string(), 1)].iter().cloned().collect();
}

// Next we have to update this call function.
// In the future object system will be redesigned (don't know how exactly, though)
pub fn call(function_name: FunctionName, args: Vec<object::Object>) -> object::Object {
    match function_name.as_ref() {
        "length" if Some(&(args.len() as u8)) == CORE_REGISTRY.get(&function_name) => {
            length_(args[0].clone())
        },
        "first" if Some(&(args.len() as u8)) == CORE_REGISTRY.get(&function_name) => {
            first_(args[0].clone())
        },
        _ => new_error(format!(
            "wrong number of arguments: got={}, expected={}",
            args.len(),
            CORE_REGISTRY
                .get(&function_name)
                .expect("Cannot find function in CORE_REGISTRY, TO_GREP: 74392761423")
        )),
    }
}

pub fn length_(str: object::Object) -> object::Object {
    match str {
        object::Object::Stringl(str) => object::Object::Integer(object::Integer {
            value: str.value.len() as i32,
        }),
        object::Object::Array(arr) => object::Object::Integer(object::Integer {
            value: arr.elements.len() as i32,
        }),
        _ => new_error(format!(
            "argument to `length` not supported, got {}",
            str.object_type()
        )),
    }
}

pub fn first_(arr: object::Object) -> object::Object {
    match arr {
        object::Object::Array(arr) => {
            if arr.elements.len() > 0 {
                arr.elements[0].clone()
            } else {
                crate::evaluation::evaluator::NIL
            }
        ),
        _ => new_error(format!(
            "argument to `first` must be array, got {}",
            arr.object_type()
        )),
    }
}
