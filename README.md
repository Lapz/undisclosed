# Undisclosed Compiler

[![Build Status](https://travis-ci.org/Lapz/underscore.svg?branch=master)](https://travis-ci.org/Lapz/Undisclosed)

The compiler for the Undisclosed programming language. It is written in RUST and will be a statically interpreted language with a vm.

# Building
UUndisclosedC is written in Rust, so you'll need to grab a recent version of the [rust compiler](https://rustup.rs/) in order to compile it. 

```bash
$ git clone https://github.com/Lapz/undisclosed
$ cd undisclosed
$ cargo install
$ undisclosed foo.us
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

# Assembly output 

Basic compiling to assembly has been added.
i.e
```rust
fn main() {
    1 <1;
    let x = 10;
    x = 1;
    x and 1;

}
```
compiles to 
```asm
.text 
	.global _main
_main:
	pushq %rbp
	movq %rsp,%rbp
	subq $16, %rsp #pro
	movq $0, %rax
	movq %rax,-8(%rbp)
	jmp .l2 
.l1:
	movq -8(%rbp),%rax
	pushq %rax
	movq $1, %rax
	popq %rdx
	addq %rdx,%rax
	movq %rax,-8(%rbp)
	movq -8(%rbp),%rax
	pushq %rax
	popq %rsi
	leaq .l4(%rip),%rax
	pushq %rax
	popq %rdi
	callq _printf
.l2:
	movq -8(%rbp),%rax
	movq $10, %rdx
	cmpq %rdx, %rax #compute e1 < e2, set ZF 
 	jl .l1
	addq $8,%rsp
	movq %rbp, %rsp #epi
	popq %rbp  
	ret
.l4:
	.asciz "%ld\n"
```



# TODO
* ~~Add support for recursive types~~
* ~~Add proper printing of types~~
* ~~Add field access~~
* Add codegen 
	[x] Compile for loops, ~~Needs fixing~~
	[x] Compile while loops,
	[ ] Compile structs,
	[ ] Compile arrays
	[x] Compile Function calls,
	[x] Compile extern function calls,
	
* Add escape analysis so values will be passed to a function explicity as a param. 
For each function work out how many params escape
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

