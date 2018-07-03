.text 
	.global _main
_square:
	pushq %rbp
	movq %rsp,%rbp
	subq $16, %rsp #pro
	movq %rdi,%rax
	pushq %rax
	movq %rdi,%rax
	popq %rdx
	imulq %rdx,%rax
	movq %rbp, %rsp #epi
	popq %rbp  
	ret

_fib:
	pushq %rbp
	movq %rsp,%rbp
	subq $16, %rsp #pro
	movq %rdi,%rax
	pushq %rax
	movq $0, %rax
	popq %rdx
	cmpq %rax, %rdx #compute e1 == e2, set ZF 
 	je .l2
	movq $1, %rax
	jmp .l3 
.l2:
	movq %rdi,%rax
	pushq %rax
	movq $1, %rax
	popq %rdx
	cmpq %rax,%rdx #compute e1 < e2, set ZF 
 	jl .l5
	movq $1, %rax
	jmp .l6 
.l5:
	movq %rdi,%rax
	pushq %rax
	movq $1, %rax
	popq %rdx
	subq %rdx,%rax
	pushq %rax
	popq %rdi
	movq %rdi,%rax
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
.l6:
.l3:
	movq %rbp, %rsp #epi
	popq %rbp  
	ret

_main:
	pushq %rbp
	movq %rsp,%rbp
	subq $48, %rsp #pro
	movq $0, %rax
	movq %rax,-8(%rbp)
	jmp .l8 
.l7:
	movq -8(%rbp),%rax
	pushq %rax
	movq $1, %rax
	popq %rdx
	addq %rdx,%rax
	movq %rax,-8(%rbp)
	movq -8(%rbp),%rax
	pushq %rax
	popq %rsi
	leaq .l10(%rip),%rax
	pushq %rax
	popq %rdi
	callq _printf
.l8:
	movq -8(%rbp),%rax
	pushq %rax
	movq $10, %rax
	popq %rdx
	cmpq %rax,%rdx #compute e1 < e2, set ZF 
 	jl .l7
	movq $0, %rax
	movq %rax,-16(%rbp)
	jmp .l12 
.l11:
	movq -16(%rbp),%rax
	pushq %rax
	popq %rsi
	leaq .l14(%rip),%rax
	pushq %rax
	popq %rdi
	callq _printf
	movq -16(%rbp),%rax
	pushq %rax
	movq $2, %rax
	popq %rdx
	addq %rdx,%rax
	movq %rax,-16(%rbp)
.l12:
	movq -16(%rbp),%rax
	pushq %rax
	movq $20, %rax
	popq %rdx
	cmpq %rax,%rdx #compute e1 < e2, set ZF 
 	jl .l11
	addq $8,%rsp
	movq $10, %rax
	pushq %rax
	popq %rdi
	callq _fib
	movq %rax,-24(%rbp)
	movq -24(%rbp),%rax
	pushq %rax
	popq %rsi
	leaq .l15(%rip),%rax
	pushq %rax
	popq %rdi
	callq _printf
	addq $16,%rsp
	movq %rbp, %rsp #epi
	popq %rbp  
	ret
.l15:
	.asciz "%ld\n"
.l10:
	.asciz "%ld\n"
.l14:
	.asciz "%ld\n"
