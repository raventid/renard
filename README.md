![alt text](https://raw.githubusercontent.com/raventid/renard/master/misc/logo.png "renard language")

Simple programming language with first class functions.

# Readme

## Roadmap
- [x] Lexer
- [x] Parser
- [x] Simple Interpreter
- [x] REPL
- [ ] Super basic standard library
- [ ] Virtual Machine with bytecode
- [ ] Low pause GC
- [ ] AOT compilation (via Rust as IR)
- [ ] Basic concurrency via Fiber (aka stackful coroutines)
- [ ] Advanced concurrency (structured concurrency)
- [ ] Runtime optimizations
- [ ] Standard library, tailored for network applications
- [x] Find a better name
- [ ] Easy modal execution (REPL(interpreter), VM, AOT)
- [ ] Basic runtime optimizations (TCO, DCE)
- [ ] Friendly error messages

## Intro
```java
// Bindings
let a = 10;
let b = 20;
let c = a + b;

// Strings
let create_greeting = fn(str, str') { str + str' };
let greeting = create_greeting("Hello, ", "Raventid");

// Higher order functions
let build_adder = fn(val) { fun(arg) { arg + val } };
let add2 = build_adder(2);
```
