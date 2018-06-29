.text 
	.global _main
_main:
	pushq %rbp
	movq %rsp,%rbp
	subq $16, %rsp #pro
	movq $0, %rax
	movq %rax, -4(%rbp)
	leaq .l1(%rip),%rax
	movq %rax,%rdi
	movq -4(%rbp),%rax
	movq %rax,%rsi
	callq _printf
	movq $10, %rax
	movq %rax, -8(%rbp)
	leaq .l2(%rip),%rax
	movq %rax,%rdi
	movq -8(%rbp),%rax
	movq %rax,%rsi
	callq _printf
	addq $4,%rsp
	leaq .l3(%rip),%rax
	movq %rax,%rdi
	movq -4(%rbp),%rax
	movq %rax,%rsi
	callq _printf
	jmp .l5 
.l4:
	movq -4(%rbp),%rax
	pushq %rax
	movq $1, %rax
	popq %rdx
	addq %rdx,%rax
	leaq .l7(%rip),%rax
	movq %rax,%rdi
	movq -4(%rbp),%rax
	movq %rax,%rsi
	callq _printf
.l5:
	movq -4(%rbp),%rax
	pushq %rax
	movq $10, %rax
	popq %rdx
	cmpq %rdx, %rax #compute e1 < e2, set ZF 
 	movq $0, %rax #zero out EAX without changing ZF 
 	setl %al #set AL register (the lower byte of EAX) to 1 iff e1 | e2 != 0 
 	jge .l4
	addq $4,%rsp
	movq %rbp, %rsp #epi
	popq %rbp  
	ret
.l1:
	.asciz "%d\n"
.l2:
	.asciz "%d\n"
.l3:
	.asciz "%d\n"
.l7:
	.asciz "%d\n"
