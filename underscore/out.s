.text 
	.global _main
_square:
	pushq %rbp
	movq %rsp,%rbp
	subq $16, %rsp #pro
	movq %rdi,%rax
	pushq %rax
	movq %rdi,%rax
	popq %rdx
	imulq %rdx,%rax
	movq %rbp, %rsp #epi
	popq %rbp  
	ret

_fib:
	pushq %rbp
	movq %rsp,%rbp
	subq $16, %rsp #pro
	movq %rdi,%rax
	pushq %rax
	movq $0, %rax
	popq %rdx
	cmpq %rax, %rdx #compute e1 == e2, set ZF 
 	pushq %rax
	movq %rdi,%rax
	pushq %rax
	movq $1, %rax
	popq %rdx
	cmpq %rax, %rdx #compute e1 == e2, set ZF 
 	popq %rdx
	orq %rdx, %rax #compute e1 | e2, set ZF 
 	movq $0, %rax #zero out EAX without changing ZF 
 	setne %al #set AL register (the lower byte of EAX) to 1 iff e1 | e2 != 0 
 	je .l2
	movq %rdi,%rax
	jmp .l3 
.l2:
	movq %rdi,%rax
	pushq %rax
	movq $1, %rax
	popq %rdx
	subq %rdx,%rax
	pushq %rax
	popq %rdi
	movq %rdi,%rax
	pushq %rax
	movq $2, %rax
	popq %rdx
	subq %rdx,%rax
	pushq %rax
	popq %rdi
	callq _fib
	pushq %rax
	callq _fib
	popq %rdx
	addq %rdx,%rax
.l3:
	movq %rbp, %rsp #epi
	popq %rbp  
	ret

_main:
	pushq %rbp
	movq %rsp,%rbp
	subq $16, %rsp #pro
	movq $5, %rax
	pushq %rax
	popq %rdi
	callq _fib
	movq %rax,-8(%rbp)
	movq -8(%rbp),%rax
	pushq %rax
	popq %rsi
	leaq .l4(%rip),%rax
	pushq %rax
	popq %rdi
	callq _printf
	addq $8,%rsp
	movq %rbp, %rsp #epi
	popq %rbp  
	ret
.l4:
	.asciz "%ld\n"
