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
	subq $80, %rsp #pro
	movq $0, %rax
	movq %rax,-8(%rbp)
	movq $1, %rax
	movq %rax,-16(%rbp)
	movq $1, %rax
	movq %rax,-24(%rbp)
	movq $2, %rax
	movq %rax,-32(%rbp)
	jmp .l2 
.l1:
	movq -8(%rbp),%rax
	pushq %rax
	movq -16(%rbp),%rax
	popq %rdx
	addq %rdx,%rax
	movq %rax,-24(%rbp)
	movq -16(%rbp),%rax
	movq %rax,-8(%rbp)
	movq -24(%rbp),%rax
	movq %rax,-16(%rbp)
	movq -24(%rbp),%rax
	pushq %rax
	popq %rsi
	leaq .l4(%rip),%rax
	pushq %rax
	popq %rdi
	callq _printf
	movq -32(%rbp),%rax
	pushq %rax
	movq $1, %rax
	popq %rdx
	addq %rdx,%rax
	movq %rax,-32(%rbp)
.l2:
	movq -32(%rbp),%rax
	pushq %rax
	movq %rdi,%rax
	popq %rdx
	cmpq %rdx, %rax #compute e1 <= e2, set ZF 
 	movq $0, %rax #zero out EAX without changing ZF 
 	setle %al #set AL register (the lower byte of EAX) to 1 iff e1 | e2 != 0 
 	jge .l1
	addq $8,%rsp
	addq $24,%rsp
	movq %rbp, %rsp #epi
	popq %rbp  
	ret
.l4:
	.asciz "%ld\n"

_main:
	pushq %rbp
	movq %rsp,%rbp
	subq $80, %rsp #pro
	movq $0, %rax
	movq %rax,-40(%rbp)
	movq $1, %rax
	movq %rax,-48(%rbp)
	movq $1, %rax
	movq %rax,-56(%rbp)
	movq $20, %rax
	movq %rax,-64(%rbp)
	movq $2, %rax
	movq %rax,-72(%rbp)
	jmp .l6 
.l5:
	movq -40(%rbp),%rax
	pushq %rax
	movq -48(%rbp),%rax
	popq %rdx
	addq %rdx,%rax
	movq %rax,-56(%rbp)
	movq -48(%rbp),%rax
	movq %rax,-40(%rbp)
	movq -56(%rbp),%rax
	movq %rax,-48(%rbp)
	movq -56(%rbp),%rax
	pushq %rax
	popq %rsi
	leaq .l8(%rip),%rax
	pushq %rax
	popq %rdi
	callq _printf
	movq -72(%rbp),%rax
	pushq %rax
	movq $1, %rax
	popq %rdx
	addq %rdx,%rax
	movq %rax,-72(%rbp)
.l6:
	movq -72(%rbp),%rax
	pushq %rax
	movq -64(%rbp),%rax
	popq %rdx
	cmpq %rdx, %rax #compute e1 <= e2, set ZF 
 	movq $0, %rax #zero out EAX without changing ZF 
 	setle %al #set AL register (the lower byte of EAX) to 1 iff e1 | e2 != 0 
 	jg .l5
	addq $8,%rsp
	movq $10, %rax
	pushq %rax
	popq %rdi
	callq _fib
	addq $32,%rsp
	movq %rbp, %rsp #epi
	popq %rbp  
	ret
.l8:
	.asciz "%ld\n"
