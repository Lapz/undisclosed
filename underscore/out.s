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
	; pushq %rax
	movq $1, %rax
	popq %rdx
	subq %rdx,%rax
	movq %rax,%rdi
	movq %rax, %rdi
	pushq %rax
	movq $2, %rax
	popq %rdx
	subq %rdx,%rax
	movq %rax,%rdi
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
	subq $32, %rsp #pro
	movq $10, %rax
	movq %rax, -8(%rbp)
	movq %rax,%rdi
	callq _square
	movq %rax,-8(%rbp)
	movq $1, %rax
	pushq %rax
	movq $10, %rax
	popq %rdx
	cmpq %rdx, %rax #compute e1 < e2, set ZF 
 	jl .l1
	movq $10, %rax
	movq %rax, -8(%rbp)
	leaq .l2(%rip),%rax
	movq %rax,%rdi
	movq -8(%rbp),%rax
	movq %rax,%rsi
	callq _printf
.l1:
	movq $10, %rax
	movq %rax, -16(%rbp)
	movq %rax,%rdi
	callq _fib
	movq %rax,-16(%rbp)
	leaq .l3(%rip),%rax
	movq %rax,%rdi
	movq -16(%rbp),%rax
	movq %rax,%rsi
	callq _printf
	addq $16,%rsp
	movq %rbp, %rsp #epi
	popq %rbp  
	ret
.l2:
	.asciz "%ld\n"
.l3:
	.asciz "%ld\n"
