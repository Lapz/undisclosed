.text 
	.global _main
_fib:
	pushq %rbp
	movq %rsp,%rbp
	subq $80, %rsp #pro
	movq $0, %rax #0 
	movq %rax,-8(%rbp)
	movq $1, %rax #1 
	movq %rax,-16(%rbp)
	movq $1, %rax #1 
	movq %rax,-24(%rbp)
	movq $1, %rax #1 
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
	movq $1, %rax #1 
	popq %rdx
	addq %rdx,%rax
	movq %rax,-32(%rbp)
.l2:
	movq -32(%rbp),%rax
	pushq %rax
	movq %rdi,%rax
	popq %rdx
	cmpq %rax,%rdx #compute e1 < e2, set ZF 
 	jl .l1
	addq $8,%rsp
	addq $24,%rsp
	movq %rbp, %rsp #epi
	popq %rbp  
	ret
.l4:
	.asciz "%ld\n"
/*
locals:{
    t5: -32,
    t3: -16,
    t2: -8,
    t4: -24
} ,
params:{
    t1: RDI
}
*/
_main:
	pushq %rbp
	movq %rsp,%rbp
	subq $64, %rsp #pro
	movq $0, %rax #0 
	movq %rax,-40(%rbp)
	movq $1, %rax #1 
	movq %rax,-48(%rbp)
	movq $1, %rax #1 
	movq %rax,-56(%rbp)
	movq $1, %rax #1 
	movq %rax,-64(%rbp)
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
	movq -64(%rbp),%rax
	pushq %rax
	movq $1, %rax #1 
	popq %rdx
	addq %rdx,%rax
	movq %rax,-64(%rbp)
.l6:
	movq -64(%rbp),%rax
	pushq %rax
	movq $10, %rax #10 
	popq %rdx
	cmpq %rax,%rdx #compute e1 < e2, set ZF 
 	jl .l5
	addq $8,%rsp
	addq $24,%rsp
	movq %rbp, %rsp #epi
	popq %rbp  
	ret
.l8:
	.asciz "%ld\n"
/*
locals:{
    t24: -56,
    t23: -48,
    t22: -40,
    t25: -64
} ,
params:{}
*/