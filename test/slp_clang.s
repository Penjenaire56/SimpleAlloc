	.text
	.file	"slp_630.c"
	.globl	g                       # -- Begin function g
	.p2align	4, 0x90
	.type	g,@function
g:                                      # @g
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	pushq	%r15
	.cfi_def_cfa_offset 24
	pushq	%r14
	.cfi_def_cfa_offset 32
	pushq	%r13
	.cfi_def_cfa_offset 40
	pushq	%r12
	.cfi_def_cfa_offset 48
	pushq	%rbx
	.cfi_def_cfa_offset 56
	.cfi_offset %rbx, -56
	.cfi_offset %r12, -48
	.cfi_offset %r13, -40
	.cfi_offset %r14, -32
	.cfi_offset %r15, -24
	.cfi_offset %rbp, -16
	movq	%rdx, -8(%rsp)          # 8-byte Spill
	movq	%rdi, %rcx
	movq	(%rdi), %rax
	movq	%rax, -32(%rsp)         # 8-byte Spill
	movq	8(%rdi), %rdx
	movq	%rdx, -16(%rsp)         # 8-byte Spill
	movq	24(%rdi), %r10
	movq	32(%rdi), %r11
	movq	(%rsi), %rdi
	movq	8(%rsi), %rbx
	movq	%rbx, -56(%rsp)         # 8-byte Spill
	movq	24(%rsi), %r13
	movq	32(%rsi), %r14
	movq	%r10, %r8
	xorq	%rax, %r8
	movq	%r11, %r9
	xorq	%rdx, %r9
	movq	%r13, %r15
	xorq	%rdi, %r15
	movq	%r14, %rbp
	xorq	%rbx, %rbp
	movq	%rbp, %rdx
	andq	%r8, %rdx
	movq	%r15, %rax
	andq	%r9, %rax
	xorq	%rdx, %rax
	movq	%rax, -40(%rsp)         # 8-byte Spill
	movq	16(%rsi), %rax
	movq	%rax, -64(%rsp)         # 8-byte Spill
	movq	40(%rsi), %rbx
	movq	%rbx, %rdx
	xorq	%rax, %rdx
	movq	%r15, %rax
	andq	%r8, %rax
	movq	%rax, -48(%rsp)         # 8-byte Spill
	movq	%rdx, %rax
	movq	%rdx, %rsi
	andq	%r8, %rdx
	movq	%rdx, -24(%rsp)         # 8-byte Spill
	andq	%r9, %rax
	movq	%rax, -72(%rsp)         # 8-byte Spill
	movq	%rbp, %rdx
	andq	%r9, %rbp
	movq	16(%rcx), %rax
	movq	40(%rcx), %r9
	movq	%r13, %r12
	andq	%r10, %r12
	movq	%r14, %rcx
	andq	%r10, %rcx
	movq	%rcx, -80(%rsp)         # 8-byte Spill
	movq	%r13, %r8
	andq	%r11, %r8
	andq	%rbx, %r10
	movq	%r10, -88(%rsp)         # 8-byte Spill
	movq	%r14, %rcx
	andq	%r11, %rcx
	movq	%rcx, -96(%rsp)         # 8-byte Spill
	andq	%r9, %r13
	andq	%rbx, %r11
	movq	%r11, -104(%rsp)        # 8-byte Spill
	andq	%r9, %r14
	andq	%r9, %rbx
	movq	%rax, %r10
	xorq	%rax, %r9
	andq	%r9, %rdx
	andq	%r9, %rsi
	movq	%rsi, -120(%rsp)        # 8-byte Spill
	andq	%r9, %r15
	xorq	%rbp, %r15
	movq	%r15, -112(%rsp)        # 8-byte Spill
	movq	%rdi, %rbp
	movq	-32(%rsp), %r11         # 8-byte Reload
	andq	%r11, %rbp
	xorq	%rdx, %rbp
	xorq	-72(%rsp), %rbp         # 8-byte Folded Reload
	movq	-64(%rsp), %rsi         # 8-byte Reload
	movq	%rsi, %r9
	movq	-16(%rsp), %rax         # 8-byte Reload
	andq	%rax, %r9
	movq	-56(%rsp), %rdx         # 8-byte Reload
	movq	%rdx, %r15
	andq	%r10, %r15
	xorq	%r9, %r15
	xorq	%r15, %r12
	movq	%rdx, %r9
	movq	%rdx, %rcx
	andq	%r11, %r9
	movq	%rdi, %r15
	andq	%rax, %r15
	xorq	%r9, %r15
	xorq	-120(%rsp), %r15        # 8-byte Folded Reload
	movq	%rsi, %rdx
	andq	%r10, %rdx
	xorq	%rdx, %r8
	xorq	-80(%rsp), %r8          # 8-byte Folded Reload
	andq	%r11, %rsi
	andq	%rax, %rcx
	andq	%r10, %rdi
	xorq	%rcx, %rdi
	xorq	%rsi, %rdi
	xorq	-96(%rsp), %r13         # 8-byte Folded Reload
	xorq	-88(%rsp), %r13         # 8-byte Folded Reload
	xorq	-104(%rsp), %r14        # 8-byte Folded Reload
	xorq	-48(%rsp), %r14         # 8-byte Folded Reload
	xorq	-40(%rsp), %rbx         # 8-byte Folded Reload
	xorq	%rbp, %r12
	xorq	%rbp, %r14
	xorq	%r15, %r8
	xorq	%r15, %rbx
	xorq	%rdi, %r13
	xorq	-112(%rsp), %rdi        # 8-byte Folded Reload
	xorq	-24(%rsp), %rdi         # 8-byte Folded Reload
	movq	-8(%rsp), %rdx          # 8-byte Reload
	movq	%r12, (%rdx)
	movq	%r8, 8(%rdx)
	movq	%r13, 16(%rdx)
	movq	%r14, 24(%rdx)
	movq	%rbx, 32(%rdx)
	movq	%rdi, 40(%rdx)
	popq	%rbx
	.cfi_def_cfa_offset 48
	popq	%r12
	.cfi_def_cfa_offset 40
	popq	%r13
	.cfi_def_cfa_offset 32
	popq	%r14
	.cfi_def_cfa_offset 24
	popq	%r15
	.cfi_def_cfa_offset 16
	popq	%rbp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	g, .Lfunc_end0-g
	.cfi_endproc
                                        # -- End function
	.ident	"clang version 10.0.0-4ubuntu1 "
	.section	".note.GNU-stack","",@progbits
