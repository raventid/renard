# clojurium

[![Build Status](https://travis-ci.org/raventid/clojurium.svg?branch=master)](https://travis-ci.org/raventid/clojurium)
[![Coverage Status](https://coveralls.io/repos/github/raventid/clojurium/badge.svg?branch=master)](https://coveralls.io/github/raventid/clojurium?branch=master)

Simple programming language with first class functions.

This project has nothing to do with Clojure programming language. Name clojurium has been used as a reference to the concept of closure.

## Roadmap
- [x] Lexer
- [x] Parser
- [ ] Simple Interpreter
- [ ] REPL
- [ ] Super basic standard library
- [ ] Virtual Machine with bytecode
- [ ] Low pause GC
- [ ] AOT compilation (via Rust as IR)
- [ ] Basic concurrency via Fiber (aka stackful coroutines)
- [ ] Advanced concurrency (structured concurrency)
- [ ] Runtime optimizations
- [ ] Standard library, tailored for network applications
- [ ] Find a better name
- [ ] Easy modal execution (REPL(interpreter), VM, AOT)
- [ ] Basic runtime optimizations (TCO, DCE)
- [ ] Friendly error messages

## Intro
```
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
