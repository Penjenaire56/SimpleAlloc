	.att_syntax
	.text
	.p2align	5
	.globl	_p
	.globl	p
_p:
p:
	movq	%rsp, %rax
	leaq	-136(%rsp), %rsp
	andq	$-8, %rsp
	movq	%rax, 88(%rsp)
	movq	%rbx, 96(%rsp)
	movq	%rbp, 104(%rsp)
	movq	%r12, 112(%rsp)
	movq	%r13, 120(%rsp)
	movq	%r14, 128(%rsp)
	movq	24(%rdi), %r10
	movq	(%rdi), %rax
	movq	%rax, %rbp
	xorq	%r10, %rbp
	movq	8(%rdi), %rcx
	movq	32(%rdi), %r8
	movq	%rcx, %r9
	movq	%r10, (%rsp)
	xorq	%r8, %r9
	movq	40(%rdi), %r10
	movq	16(%rdi), %r11
	movq	%r11, %rdi
	xorq	%r10, %rdi
	movq	24(%rsi), %r12
	movq	%r11, 8(%rsp)
	movq	(%rsi), %r11
	movq	%r11, %r13
	movq	%rbp, 16(%rsp)
	xorq	%r12, %r13
	movq	8(%rsi), %rbp
	movq	32(%rsi), %r14
	movq	%rbp, %rbx
	movq	%r12, 24(%rsp)
	xorq	%r14, %rbx
	movq	%r13, 32(%rsp)
	movq	40(%rsi), %r12
	movq	%r14, 40(%rsp)
	movq	16(%rsi), %rsi
	movq	%rsi, %r13
	movq	%rsi, 48(%rsp)
	xorq	%r12, %r13
	movq	%rdi, %r14
	andq	%rbx, %r14
	movq	%r9, %rsi
	andq	%r13, %rsi
	xorq	%r14, %rsi
	movq	%rax, %r14
	andq	%r11, %r14
	movq	%r13, 56(%rsp)
	xorq	%r14, %rsi
	movq	%rax, %r14
	andq	%rbp, %r14
	movq	%rcx, %r13
	andq	%r11, %r13
	movq	%r11, 64(%rsp)
	xorq	%r14, %r13
	movq	%r8, %r14
	andq	%r12, %r14
	movq	%r10, %r11
	andq	40(%rsp), %r11
	movq	%r10, 72(%rsp)
	xorq	%r14, %r11
	movq	16(%rsp), %r10
	andq	%rbx, %r10
	movq	%r9, %r14
	andq	32(%rsp), %r14
	movq	%rbx, 80(%rsp)
	xorq	%r14, %r10
	movq	%rcx, %rbx
	andq	48(%rsp), %rbx
	movq	8(%rsp), %r14
	andq	%rbp, %r14
	xorq	%r14, %rbx
	movq	(%rsp), %r14
	andq	24(%rsp), %r14
	xorq	%r14, %rbx
	xorq	%rsi, %rbx
	movq	%rbx, (%rdx)
	movq	8(%rsp), %rbx
	andq	48(%rsp), %rbx
	movq	(%rsp), %r14
	andq	40(%rsp), %r14
	xorq	%r14, %rbx
	movq	%r8, %r14
	andq	24(%rsp), %r14
	xorq	%r14, %rbx
	movq	%rdi, %r14
	andq	56(%rsp), %r14
	xorq	%r14, %r13
	xorq	%r13, %rbx
	movq	%rbx, 8(%rdx)
	movq	(%rsp), %rbx
	andq	%r12, %rbx
	andq	40(%rsp), %r8
	xorq	%r8, %rbx
	andq	%rbp, %rcx
	andq	48(%rsp), %rax
	xorq	%rcx, %rax
	movq	8(%rsp), %rcx
	andq	64(%rsp), %rcx
	xorq	%rcx, %rax
	movq	24(%rsp), %rcx
	andq	72(%rsp), %rcx
	xorq	%rcx, %rbx
	xorq	%rax, %rbx
	movq	%rbx, 16(%rdx)
	movq	16(%rsp), %rcx
	andq	32(%rsp), %rcx
	xorq	%rcx, %r11
	xorq	%rsi, %r11
	movq	%r11, 24(%rdx)
	movq	72(%rsp), %rcx
	andq	%r12, %rcx
	xorq	%rcx, %r13
	xorq	%r10, %r13
	movq	%r13, 32(%rdx)
	andq	32(%rsp), %rdi
	xorq	%rdi, %rax
	andq	80(%rsp), %r9
	movq	16(%rsp), %rcx
	andq	56(%rsp), %rcx
	movq	%rcx, 56(%rsp)
	movq	56(%rsp), %rcx
	xorq	%r9, %rcx
	movq	%rcx, 56(%rsp)
	xorq	56(%rsp), %rax
	movq	%rax, 56(%rsp)
	movq	56(%rsp), %rax
	movq	%rax, 40(%rdx)
	movq	96(%rsp), %rbx
	movq	104(%rsp), %rbp
	movq	112(%rsp), %r12
	movq	120(%rsp), %r13
	movq	128(%rsp), %r14
	movq	88(%rsp), %rsp
	ret 