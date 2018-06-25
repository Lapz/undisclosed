.text 
	.global _main
_square:
	pushq %rbp
	movq %rsp, %rbp
	movl %edi, -20(%rbp) #pro
