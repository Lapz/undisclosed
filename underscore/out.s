.text 
	.global _main
_main:
	pushq %rbp
	movq %rsp,%rbp
	subq $0, %rsp #pro
	movq $10, %rax
	pushq %rax
	movq $10, %rax
	popq %rdx
	orq %rdx, %rax #compute e1 | e2, set ZF 
 	movq %rbp, %rsp #epi
	popq %rbp  
	ret
