.text 
	.global _main
_fib:
	pushq %rbp
	movq %rsp,%rbp
	subq $16, %rsp #pro
	movq -8(%rbp),%rax
	pushq %rax
	movq $0, %rax
	popq %rdx
	cmpq %rax, %rdx #compute e1 == e2, set ZF 
 	pushq %rax
	cmpq $0, %rax 
je .l5
	movq -8(%rbp),%rax
	pushq %rax
	movq $1, %rax
	popq %rdx
	cmpq %rax, %rdx #compute e1 == e2, set ZF 
 .l5
	je .l2
	movq -8(%rbp),%rax
	jmp .l3 
.l2:
	movq -8(%rbp),%rax
	pushq %rax
	movq $1, %rax
	popq %rdx
	subq %rdx,%rax
	pushq %rax
	popq %rdi
	movq -8(%rbp),%rax
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
.l3:
	movq %rbp, %rsp #epi
	popq %rbp  
	ret

_main:
	pushq %rbp
	movq %rsp,%rbp
	subq $32, %rsp #pro
	movq $5, %rax
	movq %rax,-8(%rbp)
	movq $5, %rax
	pushq %rax
	popq %rdi
	callq _fib
	movq %rax,-16(%rbp)
	movq -16(%rbp),%rax
	pushq %rax
	popq %rsi
	leaq .l4(%rip),%rax
	pushq %rax
	popq %rdi
	callq _printf
	addq $16,%rsp
	movq %rbp, %rsp #epi
	popq %rbp  
	ret
.l4:
	.asciz "%ld\n"
