.text 
		.global _main
_main:

l3:
	pushq %rbp
	movq %rsp, %rbp
	movl %edi, -20(%rbp) #pro
	movq $10, %rax
	movq %rax, -4(%rbp)
.l1:
	movq $1, %rax
	movq $1, %rax
	movq %rax, -4(%rbp)
	jmp .l1 
.l2:
	movq %rbp, %rsp #epi
	popq %rbp  
	ret