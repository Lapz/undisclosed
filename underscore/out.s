.text 
	.global _main
_main:
	pushq %rbp
	movq %rsp,%rbp
	subq $0, %rsp #pro
	movq $0, %rax #0 
	cmpq $0, %rax 
	jne .l3
	movq $1, %rax #1 
.l3:
	pushq %rax
	popq %rsi
	leaq .l2(%rip),%rax
	pushq %rax
	popq %rdi
	callq _printf
	movq $0, %rax #0 
	jmp .l1 
.l1:
	movq %rbp, %rsp #epi
	popq %rbp  
	ret
.l2:
	.asciz "%d\n"
/*
locals:{} ,
params:{}
*/