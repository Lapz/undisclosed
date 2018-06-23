.text 
		.global _main
_main:

l3:
	pushq %rbp
	movq %rsp, %rbp
	movl %edi, -20(%rbp) #pro
	movq $10, %rax
	movq %rax, -4(%rbp)
	jg .l2
	movq $50, %rax
	movq %rax, -4(%rbp)
.l2:
	movq %rbp, %rsp #epi
	popq %rbp  
	ret