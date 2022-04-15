	.file	"slp_630.c"
	.text
	.p2align 4
	.globl	f
	.type	f, @function
f:
.LFB0:
	.cfi_startproc
	endbr64
	pushq	%r15
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	pushq	%r14
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	pushq	%r13
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	pushq	%r12
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	pushq	%rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	movq	8(%rdi), %rbp
	movq	(%rdi), %r12
	movq	16(%rdi), %rbx
	movq	24(%rdi), %r11
	movq	40(%rdi), %r8
	movq	%rbp, %r15
	movq	32(%rdi), %rcx
	movq	(%rsi), %rdi
	movq	24(%rsi), %r10
	movq	16(%rsi), %r14
	movq	40(%rsi), %r13
	movq	32(%rsi), %r9
	movq	%rcx, -56(%rsp)
	xorq	%rcx, %r15
	movq	8(%rsi), %rax
	movq	%rdi, %rcx
	movq	%r12, %rsi
	movq	%r15, -32(%rsp)
	xorq	%r11, %rsi
	xorq	%r10, %rcx
	movq	%rax, -48(%rsp)
	movq	%rsi, -40(%rsp)
	movq	%rbx, %rsi
	movq	%rcx, -24(%rsp)
	movq	%rax, %rcx
	movq	%r14, %rax
	xorq	%r8, %rsi
	xorq	%r9, %rcx
	xorq	%r13, %rax
	movq	%rsi, -72(%rsp)
	andq	%rax, %r15
	andq	%rcx, %rsi
	movq	%rax, -64(%rsp)
	movq	%rbp, %rax
	xorq	%r15, %rsi
	movq	%r12, %r15
	movq	%rcx, -16(%rsp)
	andq	%rdi, %rax
	andq	%rdi, %r15
	movq	-48(%rsp), %rcx
	andq	%rbx, %rdi
	xorq	%r15, %rsi
	movq	%r12, %r15
	andq	%r14, %r12
	movq	%rsi, -8(%rsp)
	movq	%rax, %rsi
	movq	%rbp, %rax
	andq	%rcx, %r15
	andq	%rcx, %rax
	andq	%r14, %rbp
	andq	%rbx, %rcx
	xorq	%r15, %rsi
	xorq	%rax, %r12
	xorq	%rcx, %rbp
	movq	-72(%rsp), %r15
	andq	-64(%rsp), %r15
	xorq	%rdi, %r12
	movq	%r11, %rdi
	andq	%r14, %rbx
	xorq	%r15, %rsi
	andq	%r10, %rdi
	xorq	%rdi, %rbp
	movq	-8(%rsp), %rdi
	xorq	%rdi, %rbp
	movq	%rbp, (%rdx)
	movq	-56(%rsp), %rax
	movq	%r11, %rbp
	andq	%r9, %rbp
	movq	%rax, %r14
	xorq	%rbp, %rbx
	movq	%rax, %rbp
	andq	%r10, %r14
	xorq	%r14, %rbx
	movq	-16(%rsp), %r14
	xorq	%rsi, %rbx
	andq	%r9, %rbp
	andq	%r13, %r11
	andq	%r8, %r10
	xorq	%rbp, %r11
	movq	%rbx, 8(%rdx)
	movq	-40(%rsp), %rbx
	andq	%r8, %r9
	xorq	%r10, %r11
	movq	-24(%rsp), %r10
	andq	%r13, %r8
	xorq	%r12, %r11
	movq	%r11, 16(%rdx)
	movq	%rax, %r11
	movq	%rbx, %rax
	andq	%r13, %r11
	andq	%r10, %rax
	movq	%r11, %r15
	xorq	%r9, %r15
	movq	-32(%rsp), %r9
	xorq	%rax, %r15
	movq	%rbx, %rax
	andq	-64(%rsp), %rbx
	xorq	%rdi, %r15
	movq	%r9, %rcx
	andq	%r14, %rax
	movq	%rbx, %rdi
	movq	%r15, 24(%rdx)
	movq	-72(%rsp), %r15
	andq	%r14, %r9
	andq	%r10, %rcx
	xorq	%rcx, %rax
	xorq	%r9, %rdi
	popq	%rbx
	.cfi_def_cfa_offset 48
	popq	%rbp
	.cfi_def_cfa_offset 40
	andq	%r10, %r15
	xorq	%rax, %r8
	xorq	%r15, %rdi
	xorq	%r8, %rsi
	xorq	%rdi, %r12
	movq	%rsi, 32(%rdx)
	movq	%r12, 40(%rdx)
	popq	%r12
	.cfi_def_cfa_offset 32
	popq	%r13
	.cfi_def_cfa_offset 24
	popq	%r14
	.cfi_def_cfa_offset 16
	popq	%r15
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE0:
	.size	f, .-f
	.ident	"GCC: (Ubuntu 9.4.0-1ubuntu1~20.04.1) 9.4.0"
	.section	.note.GNU-stack,"",@progbits
	.section	.note.gnu.property,"a"
	.align 8
	.long	 1f - 0f
	.long	 4f - 1f
	.long	 5
0:
	.string	 "GNU"
1:
	.align 8
	.long	 0xc0000002
	.long	 3f - 2f
2:
	.long	 0x3
3:
	.align 8
4:
