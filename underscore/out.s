.text 
		.global _main
_main:

l0:
	pushq %rbp
	movq %rsp, %rbp
	movl %edi, -20(%rbp) #pro
	movq $1, %rax
	pushq %rax
	movq $1, %rax
	popq %rdx
	cmpq %rdx, %rax #compute e1 < e2, set ZF 
 	movq $0, %rax #zero out EAX without changing ZF 
 	setl %al #set AL register (the lower byte of EAX) to 1 iff e1 | e2 != 0 
 	movq $10, %rax
	movq %rax, -4(%rbp)
	movq $1, %rax
	movq %rax, -4(%rbp)
	movq -4(%rbp),%rax
	pushq %rax
	movq $1, %rax
	popq %rdx
	cmpq $0, %rax 
	movq $0, %rax #zero out EAX without changing ZF 
 	setne %cl
	cmpq $0, %rdx 
	movq $0, %rdx #zero out EAX without changing ZF 
 	setne %al
	andb %cl,%al
	movq %rbp, %rsp #epi
	popq %rbp  
	ret