.text 
	.global _main
_main:
	pushq %rbp
	movq %rsp, %rbp
	movl %edi, -20(%rbp) #pro
	movq $10, %rax
	movq %rax, -4(%rbp)
	addq $4,%rsp
	movq $1, %rax
	movq %rax, -4(%rbp)
	movq -4(%rbp),%rax
	addq $4,%rsp
	movq %rbp, %rsp #epi
	popq %rbp  
	ret
