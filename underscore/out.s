.text 
		.global _main
_main:
	l0:
	movq $10, %rax
	pushq %rax
	movq $2, %rax
	popq %rdx
	movq %rdx, %rbx
	xorq %rdx, %rdx
	idivq %rbx
	pushq %rax
	ret