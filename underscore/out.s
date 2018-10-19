.text 
	.global _main
_fib:
	pushq %rbp
	movq %rsp,%rbp
	subq $32, %rsp #pro
	movq %rax,-8(%rbp)
	movq -8(%rbp),%rax
	pushq %rax
	movq $0, %rax #0 
	popq %rdx
	cmpq %rax,%rdx #compute e1 == e2, set ZF 
 	jne .l1
	movq $1, %rax #1 
	
.l1:
	movq $2, %rax #2 
	movq %rbp, %rsp #epi
	popq %rbp  
	ret
/*
locals:{
    t1: -8
} ,
params:{
    t1: RDI
}
*/
_main:
	pushq %rbp
	movq %rsp,%rbp
	subq $0, %rsp #pro
	movq $0, %rax #0 
	pushq %rax
	popq %rdi
	callq _fib
	pushq %rax
	popq %rsi
	leaq .l2(%rip),%rax
	pushq %rax
	popq %rdi
	callq _printf
	movq $0, %rax #0 
	pushq %rax
	popq %rdi
	callq _fib
	movq %rbp, %rsp #epi
	popq %rbp  
	ret
.l2:
	.asciz "%d\n"
/*
locals:{} ,
params:{}
*/