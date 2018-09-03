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
	cmpq %rax,%rdx #compute e1 <= e2, set ZF 
 	jle .l2
	movq $1, %rax #1 
	neg %rax
	jmp .l1 
.l2:
	movq -8(%rbp),%rax
	pushq %rax
	movq $1, %rax #1 
	popq %rdx
	cmpq %rax,%rdx #compute e1 == e2, set ZF 
 	pushq %rax
	cmpq $0, %rax 
	je .l5
	movq -8(%rbp),%rax
	pushq %rax
	movq $2, %rax #2 
	popq %rdx
	cmpq %rax,%rdx #compute e1 == e2, set ZF 
 .l5:
	jne .l3
	movq $1, %rax #1 
	jmp .l1 
.l3:
	movq -8(%rbp),%rax
	pushq %rax
	movq $1, %rax #1 
	popq %rdx
	subq %rdx,%rax
	pushq %rax
	popq %rdi
	movq -8(%rbp),%rax
	pushq %rax
	movq $2, %rax #2 
	popq %rdx
	subq %rdx,%rax
	pushq %rax
	popq %rdi
	callq _fib
	pushq %rax
	callq _fib
	popq %rdx
	addq %rdx,%rax
	jmp .l1 
.l1:
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
	movq $10, %rax #10 
	pushq %rax
	popq %rdi
	callq _fib
	movq $0, %rax #0 
	jmp .l4 
.l4:
	movq %rbp, %rsp #epi
	popq %rbp  
	ret
/*
locals:{} ,
params:{}
*/