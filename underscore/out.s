.text 
		.global _main
_main:

l2:
	pushq %rbp
	movq %rsp, %rbp
	movl %edi, -20(%rbp) #pro
	leaq .l1(%rip),%rdi
	callq _puts
	movq %rax, -4(%rbp)
	movq %rbp, %rsp #epi
	popq %rbp  
	ret
.l1:
	.asciz "a"