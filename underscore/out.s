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
	movq $1, %rax
	popq %rdx
	cmpq %rdx, %rax #compute e1 < e2, set ZF 
 	jl .l1
	movq $1, %rax
	neg %rax
.l1:
	movq %rdi,%rax
	pushq %rax
	movq $0, %rax
	popq %rdx
	cmpq %rdx, %rax #compute e1 == e2, set ZF 
 	je .l2
	movq $0, %rax
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
	movq %rbp, %rsp #epi
	popq %rbp  
	ret

_main:
	pushq %rbp
	movq %rsp,%rbp
	subq $0, %rsp #pro
	movq $20, %rax
	pushq %rax
	movq $20, %rax
	popq %rdx
	cmpq %rdx, %rax #compute e1 <= e2, set ZF 
 	jle .l4
	leaq .l6(%rip),%rax
	pushq %rax
	popq %rdi
	callq _puts
	jmp .l5 
.l4:
	leaq .l7(%rip),%rax
	pushq %rax
	popq %rdi
	callq _puts
.l5:
	movq $10, %rax
	pushq %rax
	popq %rdi
	callq _fib
	pushq %rax
	popq %rsi
	leaq .l8(%rip),%rax
	pushq %rax
	popq %rdi
	callq _printf
	movq %rbp, %rsp #epi
	popq %rbp  
	ret
.l7:
	.asciz "True"
.l8:
	.asciz "%ld\n"
.l6:
	.asciz "False"
