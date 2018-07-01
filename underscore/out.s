.text 
	.global _main
_square:
	pushq %rbp
	movq %rsp,%rbp
	subq $16, %rsp #pro
	movq %rax, %rdi
	pushq %rax
	movq %rax, %rdi
	popq %rdx
	imulq %rdx,%rax
	movq %rbp, %rsp #epi
	popq %rbp  
	ret

_fib:
	pushq %rbp
	movq %rsp,%rbp
	subq $16, %rsp #pro
	movq %rax, %rdi
	pushq %rax
	movq $1, %rax
	popq %rdx
	subq %rdx,%rax
	pushq %rax
	popq %rdi
	movq %rax, %rdi
	pushq %rax
	movq $2, %rax
	popq %rdx
	subq %rdx,%rax
	pushq %rax
	popq %rdi
	callq _fib
	pushq %rax
	callq _fib
	popq %rdx
	addq %rdx,%rax
	movq %rbp, %rsp #epi
	popq %rbp  
	ret

_main:
	pushq %rbp
	movq %rsp,%rbp
	subq $0, %rsp #pro
	movq $10, %rax
	pushq %rax
	popq %rdi
	callq _fib
	pushq %rax
	popq %rsi
	leaq .l1(%rip),%rax
	pushq %rax
	popq %rdi
	callq _printf
	movq %rbp, %rsp #epi
	popq %rbp  
	ret
.l1:
	.asciz "%ld\n"
