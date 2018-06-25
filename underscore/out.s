.text 
	.global _main
_main:
	pushq %rbp
	movq %rsp, %rbp
	movl %edi, -20(%rbp) #pro
	movq $10, %rax
	movq %rax, -4(%rbp)
	leaq .l1(%rip),%rdi
	movq -4(%rbp),%rsi
	callq _printf
	movq -4(%rbp),%rax
	addq $4,%rsp
	movq %rbp, %rsp #epi
	popq %rbp  
	ret
.l1:
	.asciz "%d"
