.text 
		.global _main
_main:

l0:
	pushq %rbp
    movq %rsp, %rbp
    movl %edi, -20(%rbp) #pro
	movq $2, %rax
	neg %rax
	movq $1, %rax
	not %rax
	movq $10, %rax
	pushq %rax
	movq $10, %rax
	popq %rdx
	imulq %rdx,%rax
	popq %rbp #epi
    ret
l1:
	pushq %rbp
    movq %rsp, %rbp
    movl %edi, -20(%rbp) #pro
	pushq %rax
	popq %rdx
	imulq %rdx,%rax
	popq %rbp #epi
    ret