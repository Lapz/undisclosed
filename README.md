# UNDERSCORE Compiler

[![Build Status](https://travis-ci.org/Lapz/underscore.svg?branch=master)](https://travis-ci.org/Lapz/underscore)

The compiler for the underscore programming language. It is written in RUST and will be a statically interpreted language with a vm.

# Building
UnderscoreC is written in Rust, so you'll need to grab a recent version of the [rust compiler](https://rustup.rs/) in order to compile it. 

```bash
$ git clone https://github.com/Lapz/underscore
$ cd underscore
$ cargo install
$ underscore foo.us
$ gcc  out.s -o out
$ ./out
```

# Features
Currently no code is genereated but basic parsing and type inference has been implemented
e.g. 
```rust

fn id<T>(v:T) -> T {
    v;
}

fn main() -> u8 {
    id::<u8>('c');
}

```
or 
```rust 

struct List<T> {
    head:T,
    body:List<T>  
}

```

# TODO
* ~~Add support for recursive types~~
* ~~Add proper printing of types~~
* ~~Add field access~~
* Add codegen 
# References

* [rust](https://github.com/rust-lang/rust)
* [plank](https://github.com/jDomantas/plank)
* [lox](http://www.craftinginterpreters.com)
* [menhir-lang](https://github.com/GeorgeKT/menhir-lang)
* [minicom](https://github.com/agatan/minicom)
* [tiger-rs](https://github.com/antoyo/tiger-rs)
* [Kaleidoscope](https://llvm.org/docs/tutorial/index.html)
* [kaleidoscope-rs](https://github.com/BookOwl/kaleidoscope-rs)
* [inko](https://gitlab.com/yorickpeterse/inko)
* [NovaLang](https://github.com/boomshroom/NovaLang)
* [gluon](https://github.com/gluon-lang/gluon)
* [dora](https://github.com/dinfuehr/dora)
* Modern Compiler Implementation in [ML](http://www.cs.princeton.edu/~appel/modern/ml/), [java](http://www.cs.princeton.edu/~appel/modern/java/) and [C](https://www.cs.princeton.edu/~appel/modern/c/)
 * [Developing Statically Typed Programming Language](http://blog.mgechev.com/2017/08/05/typed-lambda-calculus-create-type-checker-transpiler-compiler-javascript/)
* [/r/ProgrammingLanguages](https://www.reddit.com/r/ProgrammingLanguages/)
* [awesome-compilers](https://github.com/aalhour/awesome-compilers)

