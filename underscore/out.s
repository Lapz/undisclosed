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

_main:
	pushq %rbp
	movq %rsp,%rbp
	subq $16, %rsp #pro
	movq $10, %rax
	movq %rax, -8(%rbp)
	movq %rax,%rdi
	callq _square
	movq %rax,-8(%rbp)
	jmp .l2 
.l1:
	leaq .l4(%rip),%rax
	movq %rax,%rdi
	movq -8(%rbp),%rax
	movq %rax,%rsi
	callq _printf
	movq -8(%rbp),%rax
	pushq %rax
	movq $1, %rax
	popq %rdx
	addq %rdx,%rax
.l2:
	movq -8(%rbp),%rax
	pushq %rax
	movq $10, %rax
	popq %rdx
	cmpq %rdx, %rax #compute e1 < e2, set ZF 
 	jl .l1
	addq $8,%rsp
	movq %rbp, %rsp #epi
	popq %rbp  
	ret
.l4:
	.asciz "%ld\n"
