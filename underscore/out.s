.text 
	.global _main
_main:
	pushq %rbp
	movq %rsp,%rbp
	movl %edi, -52(%rbp) #pro
	movq $0, %rax
	movq %rax, -48(%rbp)
	movq $1, %rax
	movq %rax, -44(%rbp)
	movq $2, %rax
	movq %rax, -40(%rbp)
	movq $3, %rax
	movq %rax, -36(%rbp)
	movq $4, %rax
	movq %rax, -32(%rbp)
	movq $5, %rax
	movq %rax, -28(%rbp)
	movq $6, %rax
	movq %rax, -24(%rbp)
	movq $7, %rax
	movq %rax, -20(%rbp)
	movq $8, %rax
	movq %rax, -16(%rbp)
	movq $9, %rax
	movq %rax, -12(%rbp)
	movq $9, %rax
	movq %rax, -8(%rbp)
	addq $4,%rsp
	movq %rbp, %rsp #epi
	popq %rbp  
	ret
