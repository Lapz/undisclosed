.text 
	.global _main
	.extern strcat

_add:
	pushq %rbp
	movq %rsp,%rbp
	subq $64, %rsp #pro
	movq %rax,-24(%rbp)
	movq %rax,-32(%rbp)
	movq -24(%rbp),%rax
	pushq %rax
	movq -32(%rbp),%rax
	popq %rdx
	addq %rdx,%rax
	jmp .l2 
.l2:
	movq %rbp, %rsp #epi
	popq %rbp  
	ret
/*
locals:{
    t4: -24,
    t5: -32
} ,
params:{
    t4: RDI,
    t5: RSI
}
*/
_main:
	pushq %rbp
	movq %rsp,%rbp
	subq $16, %rsp #pro
	leaq .l4(%rip),%rax
	pushq %rax
	popq %rsi
	leaq .l5(%rip),%rax
	pushq %rax
	popq %rsi
	leaq .l6(%rip),%rax
	pushq %rax
	popq %rdi
	callq _concat
	pushq %rax
	popq %rdi
	callq _concat
	movq %rax,-40(%rbp)
	movq -40(%rbp),%rax
	pushq %rax
	popq %rdi
	callq _puts
	movq $0, %rax #0 
	jmp .l3 
	addq $8,%rsp
.l3:
	movq %rbp, %rsp #epi
	popq %rbp  
	ret
.l4:
	.asciz "World"
.l6:
	.asciz "Hello"
.l5:
	.asciz "a"
/*
locals:{
    t9: -40
} ,
params:{}
*/