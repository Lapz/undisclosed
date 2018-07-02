.text 
	.global _main
_main:
	pushq %rbp
	movq %rsp,%rbp
	subq $16, %rsp #pro
	movq $1, %rax
	movq %rax,-8(%rbp)
	movq -8(%rbp),%rax
	pushq %rax
	movq $10, %rax
	popq %rdx
	cmpq %rdx, %rax #compute e1 < e2, set ZF 
 	addq $8,%rsp
	movq %rbp, %rsp #epi
	popq %rbp  
	ret
