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

_main:
	pushq %rbp
	movq %rsp,%rbp
	subq $16, %rsp #pro
	movq $1, %rax
	movq %rax,-8(%rbp)
	movq -8(%rbp),%rax
	pushq %rax
	movq $3, %rax
	popq %rdx
	addq %rdx,%rax
	movq %rax,-8(%rbp)
	movq -8(%rbp),%rax
	pushq %rax
	popq %rdi
	callq _square
	pushq %rax
	popq %rsi
	leaq .l1(%rip),%rax
	pushq %rax
	popq %rdi
	callq _printf
	addq $8,%rsp
	movq %rbp, %rsp #epi
	popq %rbp  
	ret
.l1:
	.asciz "%ld\n"
