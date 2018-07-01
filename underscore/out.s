.text 
	.global _main
_square:
	pushq %rbp
	movq %rsp,%rbp
	subq $16, %rsp #pro
	movq %rax, %rdi
	pushq %rax
	movq %rax, %rdi
	popq %rdx
	imulq %rdx,%rax
	movq %rbp, %rsp #epi
	popq %rbp  
	ret

_fib:
	pushq %rbp
	movq %rsp,%rbp
	subq $16, %rsp #pro
	movq %rax, %rdi
	pushq %rax
	movq $1, %rax
	popq %rdx
	cmpq %rdx, %rax #compute e1 < e2, set ZF 
 	jl .l2
	movq $0, %rax
	jmp .l3 
.l2:
	movq %rax, %rdi
	pushq %rax
	movq $0, %rax
	popq %rdx
	cmpq %rdx, %rax #compute e1 == e2, set ZF 
 	je .l5
	movq $0, %rax
	jmp .l6 
.l5:
	movq %rax, %rdi
	pushq %rax
	movq $1, %rax
	popq %rdx
	subq %rdx,%rax
	pushq %rax
	popq %rdi
	movq %rax, %rdi
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
.l6:
.l3:
	movq %rbp, %rsp #epi
	popq %rbp  
	ret

_main:
	pushq %rbp
	movq %rsp,%rbp
	subq $0, %rsp #pro
	movq $10, %rax
	pushq %rax
	popq %rdi
	callq _square
	pushq %rax
	popq %rsi
	leaq .l7(%rip),%rax
	pushq %rax
	popq %rdi
	callq _printf
	movq %rbp, %rsp #epi
	popq %rbp  
	ret
.l7:
	.asciz "%ld\n"
