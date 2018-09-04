.text 
	.global _main
	.extern strcat

_add:
	pushq %rbp
	movq %rsp,%rbp
	subq $64, %rsp #pro
	movq %rax,-24(%rbp)
	movq %rax,-32(%rbp)
	movq -32(%rbp),%rax
	pushq %rax
	popq %rsi
	movq -24(%rbp),%rax
	pushq %rax
	popq %rdi
	callq _concat
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
    t5: RSI,
    t4: RDI
}
*/
_main:
	pushq %rbp
	movq %rsp,%rbp
	subq $0, %rsp #pro
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
	pushq %rax
	popq %rsi
	leaq .l7(%rip),%rax
	pushq %rax
	popq %rdi
	callq _printf
	movq $0, %rax #0 
	jmp .l3 
.l3:
	movq %rbp, %rsp #epi
	popq %rbp  
	ret
.l4:
	.asciz "World"
.l6:
	.asciz "Hello"
.l7:
	.asciz "%s\n"
.l5:
	.asciz " \u{0}"
/*
locals:{} ,
params:{}
*/