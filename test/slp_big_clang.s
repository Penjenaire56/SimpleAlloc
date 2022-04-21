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
	subq	$432, %rsp              # imm = 0x1B0
	.cfi_def_cfa_offset 488
	.cfi_offset %rbx, -56
	.cfi_offset %r12, -48
	.cfi_offset %r13, -40
	.cfi_offset %r14, -32
	.cfi_offset %r15, -24
	.cfi_offset %rbp, -16
	movq	%rdx, 424(%rsp)         # 8-byte Spill
	movq	%rdi, %r15
	movq	(%rdi), %rdx
	movq	8(%rdi), %rbp
	movq	%rbp, -104(%rsp)        # 8-byte Spill
	movq	16(%rdi), %r11
	movq	%r11, -128(%rsp)        # 8-byte Spill
	movq	24(%rdi), %r10
	movq	(%rsi), %r8
	movq	8(%rsi), %rbx
	movq	%rsi, %r12
	movq	%rbx, -96(%rsp)         # 8-byte Spill
	movq	16(%rsi), %rdi
	movq	%rdi, -72(%rsp)         # 8-byte Spill
	movq	32(%rsi), %r9
	movq	%rbx, %rax
	movq	%rdx, -112(%rsp)        # 8-byte Spill
	andq	%rdx, %rax
	movq	%r8, %rcx
	andq	%rbp, %rcx
	xorq	%rax, %rcx
	movq	%rcx, 240(%rsp)         # 8-byte Spill
	movq	%rdi, %rax
	andq	%rdx, %rax
	movq	%rbx, %rcx
	andq	%rbp, %rcx
	movq	%r8, %rdx
	andq	%r11, %rdx
	xorq	%rcx, %rdx
	xorq	%rax, %rdx
	movq	%rdx, 248(%rsp)         # 8-byte Spill
	movq	%rdi, %rax
	andq	%rbp, %rax
	movq	%rbx, %rcx
	andq	%r11, %rcx
	xorq	%rax, %rcx
	movq	%rcx, 400(%rsp)         # 8-byte Spill
	movq	%r9, %rax
	movq	%r10, -40(%rsp)         # 8-byte Spill
	andq	%r10, %rax
	movq	32(%r15), %rdx
	movq	%rdx, -24(%rsp)         # 8-byte Spill
	movq	24(%rsi), %rsi
	movq	%rsi, %rcx
	andq	%rdx, %rcx
	xorq	%rax, %rcx
	movq	%rcx, 176(%rsp)         # 8-byte Spill
	movq	%r9, %rax
	movq	%r9, %rbx
	movq	%r9, -120(%rsp)         # 8-byte Spill
	andq	%rdx, %rax
	movq	40(%r15), %rdi
	movq	%rdi, -64(%rsp)         # 8-byte Spill
	movq	%rsi, %rcx
	movq	%rsi, %r9
	movq	%rsi, -32(%rsp)         # 8-byte Spill
	andq	%rdi, %rcx
	xorq	%rax, %rcx
	movq	40(%r12), %rsi
	movq	%rsi, %rax
	andq	%r10, %rax
	xorq	%rax, %rcx
	movq	%rcx, 416(%rsp)         # 8-byte Spill
	movq	%rsi, %rax
	movq	%rsi, -16(%rsp)         # 8-byte Spill
	andq	%rdx, %rax
	movq	%rbx, %rcx
	andq	%rdi, %rcx
	xorq	%rax, %rcx
	movq	48(%r15), %rbp
	movq	48(%r12), %r11
	movq	%r11, %rax
	andq	%rbp, %rax
	xorq	%rcx, %rax
	movq	%rax, 392(%rsp)         # 8-byte Spill
	movq	%rsi, %rax
	andq	%rdi, %rax
	movq	%r11, %rcx
	movq	56(%r15), %rdi
	andq	%rdi, %rcx
	xorq	%rax, %rcx
	movq	56(%r12), %r14
	movq	%r14, %rax
	andq	%rbp, %rax
	xorq	%rax, %rcx
	movq	%rcx, 408(%rsp)         # 8-byte Spill
	movq	%r9, %rbx
	xorq	%r8, %rbx
	movq	%rbx, 200(%rsp)         # 8-byte Spill
	movq	%r8, 168(%rsp)          # 8-byte Spill
	xorq	%r11, %r8
	movq	%r11, %rcx
	movq	96(%r12), %rax
	xorq	%rax, %rcx
	movq	%rcx, -8(%rsp)          # 8-byte Spill
	movq	72(%r12), %r9
	movq	%r12, %r10
	movq	%rax, 232(%rsp)         # 8-byte Spill
	movq	%rax, 136(%rsp)         # 8-byte Spill
	movq	%rax, 144(%rsp)         # 8-byte Spill
	xorq	%r9, %rax
	movq	%rax, 160(%rsp)         # 8-byte Spill
	movq	%rax, 152(%rsp)         # 8-byte Spill
	movq	%rax, 128(%rsp)         # 8-byte Spill
	xorq	%rbx, %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	%rax, 72(%rsp)          # 8-byte Spill
	xorq	%r11, %rax
	movq	%rax, 96(%rsp)          # 8-byte Spill
	movq	%r11, %rax
	movq	%r14, %rcx
	movq	%rdi, -56(%rsp)         # 8-byte Spill
	andq	%rdi, %rcx
	movq	64(%r15), %r12
	andq	%r12, %rax
	xorq	%rcx, %rax
	movq	64(%r10), %rbx
	movq	%rbx, -48(%rsp)         # 8-byte Spill
	movq	%rbx, %rcx
	andq	%rbp, %rcx
	xorq	%rcx, %rax
	movq	%rax, 384(%rsp)         # 8-byte Spill
	movq	%rbx, %rcx
	andq	%rdi, %rcx
	movq	%r14, %rdx
	movq	%r12, 48(%rsp)          # 8-byte Spill
	andq	%r12, %rdx
	xorq	%rcx, %rdx
	movq	72(%r15), %rsi
	movq	%r9, %rax
	andq	%rsi, %rax
	xorq	%rdx, %rax
	movq	%rax, 352(%rsp)         # 8-byte Spill
	movq	%rbx, %rcx
	andq	%r12, %rcx
	movq	%r10, %r12
	movq	80(%r10), %rax
	movq	%rax, %rdi
	movq	%rsi, 336(%rsp)         # 8-byte Spill
	andq	%rsi, %rdi
	xorq	%rcx, %rdi
	movq	%r9, %rcx
	movq	80(%r15), %rdx
	andq	%rdx, %rcx
	xorq	%rdi, %rcx
	movq	%rcx, 376(%rsp)         # 8-byte Spill
	movq	88(%r10), %r11
	movq	%r11, %rcx
	movq	%r11, 24(%rsp)          # 8-byte Spill
	andq	%rsi, %rcx
	movq	%rax, %rdi
	andq	%rdx, %rdi
	movq	%rdx, %r10
	movq	%rdx, 328(%rsp)         # 8-byte Spill
	xorq	%rcx, %rdi
	movq	%r8, 64(%rsp)           # 8-byte Spill
	movq	%r8, 112(%rsp)          # 8-byte Spill
	movq	%r8, 120(%rsp)          # 8-byte Spill
	xorq	%r9, %r8
	movq	%r8, 104(%rsp)          # 8-byte Spill
	movq	88(%r15), %rcx
	andq	%rcx, %r9
	movq	%rcx, %r8
	movq	%rcx, 320(%rsp)         # 8-byte Spill
	xorq	%rdi, %r9
	movq	%r9, 368(%rsp)          # 8-byte Spill
	movq	104(%r12), %rdx
	movq	%r14, %rsi
	movq	%r14, 32(%rsp)          # 8-byte Spill
	movq	%r14, %rcx
	xorq	%rdx, %rcx
	movq	%rcx, %r13
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	-120(%rsp), %r14        # 8-byte Reload
	xorq	%r14, %rcx
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	movq	%rdx, %rbx
	movq	%rdx, %rcx
	movq	%rdx, %rdi
	xorq	%rax, %rdx
	movq	%rdx, -120(%rsp)        # 8-byte Spill
	movq	-96(%rsp), %rdx         # 8-byte Reload
	xorq	%rdx, %r14
	xorq	%rsi, %rdx
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	movq	%rdx, 288(%rsp)         # 8-byte Spill
	movq	%rdx, 216(%rsp)         # 8-byte Spill
	xorq	%rax, %rdx
	movq	%rdx, -96(%rsp)         # 8-byte Spill
	movq	%rax, %rdx
	andq	%r10, %r11
	andq	%r8, %rdx
	xorq	%r11, %rdx
	movq	%rdx, 88(%rsp)          # 8-byte Spill
	movq	96(%r15), %r8
	andq	%r8, %rbx
	movq	104(%r15), %r9
	movq	136(%rsp), %rax         # 8-byte Reload
	andq	%r9, %rax
	xorq	%rbx, %rax
	movq	%rax, 136(%rsp)         # 8-byte Spill
	movq	112(%r15), %rsi
	andq	%r9, %rcx
	movq	144(%rsp), %rax         # 8-byte Reload
	andq	%rsi, %rax
	xorq	%rcx, %rax
	movq	112(%r12), %rcx
	movq	%rcx, %rdx
	andq	%r8, %rdx
	xorq	%rdx, %rax
	movq	%rax, 144(%rsp)         # 8-byte Spill
	movq	%rcx, %rdx
	movq	%rcx, %rbx
	movq	%rcx, -88(%rsp)         # 8-byte Spill
	andq	%r9, %rdx
	andq	%rsi, %rdi
	movq	%rsi, %r15
	movq	%rsi, 280(%rsp)         # 8-byte Spill
	xorq	%rdx, %rdi
	movq	%rdi, 208(%rsp)         # 8-byte Spill
	movq	-8(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, %rsi
	movq	%rcx, %rdx
	movq	%rcx, -80(%rsp)         # 8-byte Spill
	movq	-32(%rsp), %rax         # 8-byte Reload
	xorq	%rax, %rcx
	movq	%rcx, -8(%rsp)          # 8-byte Spill
	movq	%rax, %rcx
	movq	-40(%rsp), %rax         # 8-byte Reload
	andq	%rax, %rcx
	movq	%rcx, -32(%rsp)         # 8-byte Spill
	movq	%rbp, %rcx
	movq	%rbp, 296(%rsp)         # 8-byte Spill
	movq	%rbp, %r11
	xorq	%r8, %r11
	movq	-48(%rsp), %r12         # 8-byte Reload
	xorq	%rbx, %r12
	andq	%r11, %rsi
	movq	%rsi, 344(%rsp)         # 8-byte Spill
	andq	%r11, %r13
	movq	%r13, 304(%rsp)         # 8-byte Spill
	movq	%r12, %rsi
	andq	%r11, %rsi
	movq	%rsi, 272(%rsp)         # 8-byte Spill
	xorq	%rax, %r11
	movq	%rax, %rbp
	xorq	-112(%rsp), %rbp        # 8-byte Folded Reload
	movq	-56(%rsp), %r10         # 8-byte Reload
	xorq	%r9, %r10
	andq	%r10, %rdx
	movq	%rdx, 224(%rsp)         # 8-byte Spill
	andq	%r10, 8(%rsp)           # 8-byte Folded Spill
	movq	%r12, %rax
	andq	%r10, %rax
	movq	%rax, 264(%rsp)         # 8-byte Spill
	movq	-24(%rsp), %rax         # 8-byte Reload
	xorq	%rax, %r10
	movq	%rax, %rdi
	xorq	-104(%rsp), %rdi        # 8-byte Folded Reload
	movq	%rdi, 192(%rsp)         # 8-byte Spill
	movq	%r14, %rax
	andq	%rbp, %rax
	movq	%rbp, 312(%rsp)         # 8-byte Spill
	movq	200(%rsp), %rdx         # 8-byte Reload
	movq	%rdx, %rsi
	andq	%rdi, %rsi
	xorq	%rax, %rsi
	movq	%rsi, -24(%rsp)         # 8-byte Spill
	movq	48(%rsp), %rbx          # 8-byte Reload
	xorq	%r15, %rbx
	andq	%rbx, -80(%rsp)         # 8-byte Folded Spill
	andq	%rbx, 16(%rsp)          # 8-byte Folded Spill
	movq	%r12, %rax
	andq	%rbx, %rax
	movq	%rax, 256(%rsp)         # 8-byte Spill
	movq	-64(%rsp), %r13         # 8-byte Reload
	xorq	%r13, %rbx
	movq	%r13, %rax
	xorq	-128(%rsp), %rax        # 8-byte Folded Reload
	movq	%rax, 184(%rsp)         # 8-byte Spill
	movq	%r14, %rdi
	movq	192(%rsp), %r13         # 8-byte Reload
	andq	%r13, %rdi
	movq	%rdx, %rsi
	andq	%rax, %rsi
	xorq	%rdi, %rsi
	movq	-16(%rsp), %rax         # 8-byte Reload
	xorq	%rax, %r12
	movq	%r12, -40(%rsp)         # 8-byte Spill
	xorq	-72(%rsp), %rax         # 8-byte Folded Reload
	movq	%rax, %rdi
	andq	%rbp, %rdi
	xorq	%rdi, %rsi
	movq	%rsi, 360(%rsp)         # 8-byte Spill
	movq	-120(%rsp), %rdx        # 8-byte Reload
	movq	%rdx, %r15
	movq	%rdx, %r12
	movq	%rdx, -64(%rsp)         # 8-byte Spill
	xorq	%r14, %rdx
	movq	%rdx, -120(%rsp)        # 8-byte Spill
	movq	%rax, %rdx
	andq	%r13, %rdx
	movq	184(%rsp), %rsi         # 8-byte Reload
	andq	%rsi, %r14
	xorq	%rdx, %r14
	movq	-112(%rsp), %r13        # 8-byte Reload
	andq	%r13, 168(%rsp)         # 8-byte Folded Spill
	xorq	%rcx, %r13
	movq	64(%rsp), %rcx          # 8-byte Reload
	andq	%r13, %rcx
	xorq	%r14, %rcx
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	movq	-88(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, -16(%rsp)         # 8-byte Spill
	xorq	24(%rsp), %rcx          # 8-byte Folded Reload
	movq	%rcx, %r14
	movq	%rcx, %rbp
	movq	%rcx, -112(%rsp)        # 8-byte Spill
	xorq	%rax, %rcx
	movq	%rcx, -88(%rsp)         # 8-byte Spill
	andq	%rsi, %rax
	movq	-104(%rsp), %rsi        # 8-byte Reload
	xorq	-56(%rsp), %rsi         # 8-byte Folded Reload
	movq	112(%rsp), %rcx         # 8-byte Reload
	andq	%rsi, %rcx
	xorq	%rax, %rcx
	movq	40(%rsp), %rax          # 8-byte Reload
	andq	%r13, %rax
	xorq	%rax, %rcx
	movq	%rcx, 112(%rsp)         # 8-byte Spill
	movq	-72(%rsp), %rdx         # 8-byte Reload
	movq	%rdx, %rcx
	movq	-128(%rsp), %rax        # 8-byte Reload
	andq	%rax, %rcx
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	xorq	48(%rsp), %rax          # 8-byte Folded Reload
	movq	%rax, -128(%rsp)        # 8-byte Spill
	movq	288(%rsp), %rdi         # 8-byte Reload
	andq	%rsi, %rdi
	movq	120(%rsp), %rcx         # 8-byte Reload
	andq	%rax, %rcx
	xorq	%rdi, %rcx
	xorq	-48(%rsp), %rdx         # 8-byte Folded Reload
	movq	%rdx, -72(%rsp)         # 8-byte Spill
	andq	%r13, %rdx
	xorq	%rdx, %rcx
	movq	%rcx, 120(%rsp)         # 8-byte Spill
	movq	304(%rsp), %rax         # 8-byte Reload
	xorq	%rax, 224(%rsp)         # 8-byte Folded Spill
	movq	-80(%rsp), %rax         # 8-byte Reload
	xorq	8(%rsp), %rax           # 8-byte Folded Reload
	xorq	272(%rsp), %rax         # 8-byte Folded Reload
	movq	%rax, -80(%rsp)         # 8-byte Spill
	movq	16(%rsp), %rdx          # 8-byte Reload
	xorq	264(%rsp), %rdx         # 8-byte Folded Reload
	andq	%r8, 232(%rsp)          # 8-byte Folded Spill
	movq	336(%rsp), %rcx         # 8-byte Reload
	xorq	%rcx, %r8
	movq	160(%rsp), %rax         # 8-byte Reload
	andq	%r8, %rax
	xorq	%rdx, %rax
	movq	%rax, 160(%rsp)         # 8-byte Spill
	andq	%r8, %r15
	xorq	256(%rsp), %r15         # 8-byte Folded Reload
	movq	328(%rsp), %rdx         # 8-byte Reload
	xorq	%rdx, %r9
	movq	152(%rsp), %rax         # 8-byte Reload
	andq	%r9, %rax
	xorq	%r15, %rax
	movq	%rax, 152(%rsp)         # 8-byte Spill
	andq	%r8, %r14
	andq	%r9, %r12
	xorq	%r14, %r12
	movq	280(%rsp), %r15         # 8-byte Reload
	andq	%r15, -16(%rsp)         # 8-byte Folded Spill
	movq	320(%rsp), %rdi         # 8-byte Reload
	xorq	%rdi, %r15
	movq	128(%rsp), %rax         # 8-byte Reload
	andq	%r15, %rax
	xorq	%r12, %rax
	movq	%rax, 128(%rsp)         # 8-byte Spill
	andq	%r9, %rbp
	movq	-64(%rsp), %r14         # 8-byte Reload
	andq	%r15, %r14
	xorq	%rbp, %r14
	xorq	%rcx, %r13
	movq	-72(%rsp), %rax         # 8-byte Reload
	movq	%rax, %rcx
	andq	%rsi, %rcx
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	xorq	%rdx, %rsi
	movq	%rax, %rcx
	movq	24(%rsp), %rdx          # 8-byte Reload
	xorq	%rdx, %rax
	movq	%rax, %rbp
	movq	%rdx, %rax
	andq	%rdi, %rax
	movq	%rax, -104(%rsp)        # 8-byte Spill
	movq	-128(%rsp), %rax        # 8-byte Reload
	andq	%rax, 216(%rsp)         # 8-byte Folded Spill
	andq	%rax, %rcx
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	xorq	%rdi, %rax
	movq	%rax, %r12
	movq	-96(%rsp), %rdi         # 8-byte Reload
	movq	%rdi, %rax
	andq	%r13, %rax
	movq	104(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, %rdx
	andq	%rsi, %rdx
	xorq	%rax, %rdx
	movq	%rdx, -128(%rsp)        # 8-byte Spill
	movq	%rcx, %rax
	andq	%r13, %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	andq	%rbp, %r13
	movq	%rdi, %rax
	andq	%rsi, %rax
	xorq	%r13, %rax
	andq	%r12, %rcx
	xorq	%rax, %rcx
	movq	%rcx, 104(%rsp)         # 8-byte Spill
	andq	%rbp, %rsi
	andq	%r12, %rdi
	movq	%rdi, %rdx
	andq	%r12, %rbp
	movq	%rbp, -72(%rsp)         # 8-byte Spill
	movq	-8(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, %rdi
	andq	%r11, %rdi
	xorq	%rdx, %rdi
	xorq	%rsi, %rdi
	movq	56(%rsp), %rsi          # 8-byte Reload
	movq	%rsi, %rdx
	andq	%r11, %rdx
	movq	%rcx, %r12
	andq	%r10, %r12
	xorq	%rdx, %r12
	movq	%rsi, %rdx
	andq	%r10, %rdx
	andq	%rbx, %rcx
	xorq	%rdx, %rcx
	movq	-40(%rsp), %rax         # 8-byte Reload
	andq	%rax, %r11
	xorq	%r11, %rcx
	movq	%rcx, -8(%rsp)          # 8-byte Spill
	andq	%rax, %r10
	andq	%rbx, %rsi
	xorq	%r10, %rsi
	movq	%rsi, 56(%rsp)          # 8-byte Spill
	andq	%rbx, %rax
	movq	%rax, -40(%rsp)         # 8-byte Spill
	movq	200(%rsp), %rax         # 8-byte Reload
	movq	312(%rsp), %rcx         # 8-byte Reload
	andq	%rcx, %rax
	movq	%rax, %r11
	xorq	%rcx, %r8
	xorq	192(%rsp), %r9          # 8-byte Folded Reload
	andq	%r15, -112(%rsp)        # 8-byte Folded Spill
	xorq	184(%rsp), %r15         # 8-byte Folded Reload
	movq	-120(%rsp), %rax        # 8-byte Reload
	movq	%rax, %rdx
	andq	%r8, %rdx
	movq	80(%rsp), %rcx          # 8-byte Reload
	andq	%r9, %rcx
	xorq	%rdx, %rcx
	movq	%rcx, 80(%rsp)          # 8-byte Spill
	movq	-88(%rsp), %rsi         # 8-byte Reload
	movq	%rsi, %rdx
	andq	%r8, %rdx
	movq	%rax, %rbp
	andq	%r9, %rbp
	xorq	%rdx, %rbp
	movq	72(%rsp), %rcx          # 8-byte Reload
	andq	%r15, %rcx
	xorq	%rbp, %rcx
	movq	%rcx, 72(%rsp)          # 8-byte Spill
	movq	%rsi, %rbp
	andq	%r9, %rbp
	movq	32(%rsp), %rdx          # 8-byte Reload
	xorq	%rax, %rdx
	andq	%r15, %rax
	xorq	%rbp, %rax
	movq	%rax, %r10
	movq	(%rsp), %rbx            # 8-byte Reload
	andq	%r8, %rbx
	movq	%rbx, (%rsp)            # 8-byte Spill
	xorq	296(%rsp), %r8          # 8-byte Folded Reload
	xorq	-56(%rsp), %r9          # 8-byte Folded Reload
	movq	-48(%rsp), %rax         # 8-byte Reload
	xorq	%rsi, %rax
	andq	%r15, %rsi
	movq	%rsi, -88(%rsp)         # 8-byte Spill
	xorq	48(%rsp), %r15          # 8-byte Folded Reload
	movq	%rdx, %rbp
	andq	%r8, %rbp
	movq	96(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, %rsi
	andq	%r9, %rsi
	xorq	%rbp, %rsi
	movq	%rsi, -56(%rsp)         # 8-byte Spill
	movq	%rcx, %rsi
	andq	%r8, %rsi
	movq	%rsi, -96(%rsp)         # 8-byte Spill
	andq	%rax, %r8
	movq	%rdx, %rbp
	andq	%r9, %rbp
	xorq	%r8, %rbp
	andq	%r15, %rcx
	xorq	%rbp, %rcx
	movq	%rcx, 96(%rsp)          # 8-byte Spill
	andq	%rax, %r9
	andq	%r15, %rdx
	xorq	%r9, %rdx
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	andq	%rax, %r15
	movq	%r10, %r13
	movq	400(%rsp), %rbp         # 8-byte Reload
	xorq	%rbp, %r10
	movq	%r10, -120(%rsp)        # 8-byte Spill
	xorq	168(%rsp), %rbp         # 8-byte Folded Reload
	movq	-32(%rsp), %r10         # 8-byte Reload
	xorq	%rbp, %r10
	movq	%r11, %rdx
	xorq	%r10, %rdx
	movq	%rdx, -48(%rsp)         # 8-byte Spill
	xorq	64(%rsp), %r10          # 8-byte Folded Reload
	movq	232(%rsp), %rsi         # 8-byte Reload
	movq	208(%rsp), %r9          # 8-byte Reload
	xorq	%r9, %rsi
	movq	88(%rsp), %rax          # 8-byte Reload
	xorq	%rsi, %rax
	xorq	%rax, %r14
	movq	%r14, -64(%rsp)         # 8-byte Spill
	xorq	160(%rsp), %rax         # 8-byte Folded Reload
	movq	392(%rsp), %r8          # 8-byte Reload
	xorq	%r8, %r10
	movq	%r10, -32(%rsp)         # 8-byte Spill
	movq	352(%rsp), %r11         # 8-byte Reload
	xorq	%r11, %rax
	movq	%rax, 88(%rsp)          # 8-byte Spill
	xorq	32(%rsp), %rdi          # 8-byte Folded Reload
	xorq	-96(%rsp), %rdi         # 8-byte Folded Reload
	xorq	%rbx, %r14
	xorq	%r10, %r14
	movq	56(%rsp), %r10          # 8-byte Reload
	xorq	%r10, %r14
	xorq	%rdi, %r14
	movq	8(%rsp), %rbx           # 8-byte Reload
	xorq	%rbx, %r13
	xorq	%rdx, %r13
	xorq	%rax, %r13
	xorq	%rdi, %r13
	xorq	344(%rsp), %rsi         # 8-byte Folded Reload
	movq	216(%rsp), %rax         # 8-byte Reload
	xorq	%r8, %rax
	xorq	%rsi, %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	xorq	%r11, %rcx
	xorq	%rax, %rcx
	movq	-120(%rsp), %rax        # 8-byte Reload
	xorq	32(%rsp), %rax          # 8-byte Folded Reload
	xorq	%r10, %rax
	movq	(%rsp), %rdx            # 8-byte Reload
	xorq	%rbx, %rdx
	xorq	%r9, %rbp
	xorq	%rbp, %rdx
	xorq	-96(%rsp), %rdx         # 8-byte Folded Reload
	xorq	%rcx, %rax
	movq	%rax, -120(%rsp)        # 8-byte Spill
	xorq	%rcx, %rdx
	movq	%rdx, (%rsp)            # 8-byte Spill
	movq	-88(%rsp), %r9          # 8-byte Reload
	movq	-128(%rsp), %rax        # 8-byte Reload
	xorq	%rax, %r9
	movq	136(%rsp), %rdi         # 8-byte Reload
	xorq	%rdi, %rax
	movq	%rax, -128(%rsp)        # 8-byte Spill
	xorq	-16(%rsp), %rdi         # 8-byte Folded Reload
	movq	-104(%rsp), %rbp        # 8-byte Reload
	xorq	%rdi, %rbp
	movq	-112(%rsp), %rcx        # 8-byte Reload
	xorq	%rbp, %rcx
	movq	%rcx, -112(%rsp)        # 8-byte Spill
	xorq	152(%rsp), %rbp         # 8-byte Folded Reload
	movq	240(%rsp), %rbx         # 8-byte Reload
	movq	40(%rsp), %rax          # 8-byte Reload
	xorq	%rbx, %rax
	movq	176(%rsp), %r10         # 8-byte Reload
	xorq	%rax, %r10
	movq	-24(%rsp), %rsi         # 8-byte Reload
	xorq	%r10, %rsi
	movq	%rsi, -24(%rsp)         # 8-byte Spill
	xorq	112(%rsp), %r10         # 8-byte Folded Reload
	xorq	%r15, %r12
	xorq	-72(%rsp), %r12         # 8-byte Folded Reload
	movq	376(%rsp), %rdx         # 8-byte Reload
	xorq	%rdx, %rbp
	movq	%rbp, -104(%rsp)        # 8-byte Spill
	movq	408(%rsp), %r8          # 8-byte Reload
	xorq	%r8, %r10
	movq	%r10, 176(%rsp)         # 8-byte Spill
	movq	-56(%rsp), %r11         # 8-byte Reload
	xorq	%r11, %r12
	xorq	%rsi, %r9
	xorq	%rbp, %r9
	xorq	%r12, %r9
	movq	-40(%rsp), %rsi         # 8-byte Reload
	xorq	%rsi, %r15
	xorq	%rcx, %rsi
	xorq	%r10, %rsi
	movq	80(%rsp), %rbp          # 8-byte Reload
	xorq	%rbp, %rsi
	xorq	%r12, %rsi
	xorq	224(%rsp), %rax         # 8-byte Folded Reload
	movq	24(%rsp), %rcx          # 8-byte Reload
	xorq	%r8, %rcx
	xorq	%rax, %rcx
	xorq	%rdx, %rcx
	movq	-128(%rsp), %rax        # 8-byte Reload
	xorq	%rbp, %rax
	xorq	%r11, %rax
	xorq	%rbx, %rdi
	xorq	-88(%rsp), %rdi         # 8-byte Folded Reload
	xorq	%rdi, %r15
	xorq	%rcx, %rax
	movq	%rax, -128(%rsp)        # 8-byte Spill
	xorq	%rcx, %r15
	movq	368(%rsp), %r10         # 8-byte Reload
	movq	144(%rsp), %rbp         # 8-byte Reload
	xorq	%rbp, %r10
	movq	384(%rsp), %rbx         # 8-byte Reload
	movq	-80(%rsp), %rax         # 8-byte Reload
	xorq	%rbx, %rax
	xorq	%r10, %rax
	movq	%rax, %r11
	movq	%rax, -80(%rsp)         # 8-byte Spill
	xorq	128(%rsp), %r10         # 8-byte Folded Reload
	movq	248(%rsp), %rdi         # 8-byte Reload
	movq	416(%rsp), %r12         # 8-byte Reload
	xorq	%rdi, %r12
	movq	360(%rsp), %rdx         # 8-byte Reload
	xorq	%r12, %rdx
	xorq	120(%rsp), %r12         # 8-byte Folded Reload
	xorq	%rbx, %r12
	movq	104(%rsp), %rbx         # 8-byte Reload
	movq	96(%rsp), %rcx          # 8-byte Reload
	xorq	%rcx, %rbx
	movq	%rdi, %rax
	movq	72(%rsp), %r8           # 8-byte Reload
	xorq	%r8, %rax
	xorq	%r11, %rax
	xorq	%rbx, %rax
	movq	%rbx, %r11
	movq	%r10, %rdi
	xorq	%rdx, %rdi
	movq	-8(%rsp), %rbx          # 8-byte Reload
	xorq	%rbx, %rdi
	xorq	%r11, %rdi
	xorq	%r8, %rcx
	xorq	%rbp, %rbx
	movq	%rbp, %r8
	xorq	%r12, %rbx
	xorq	%rbx, %rcx
	movq	424(%rsp), %rbx         # 8-byte Reload
	movq	168(%rsp), %rbp         # 8-byte Reload
	movq	%rbp, (%rbx)
	movq	240(%rsp), %rbp         # 8-byte Reload
	movq	%rbp, 8(%rbx)
	movq	-128(%rsp), %rbp        # 8-byte Reload
	movq	%rbp, 80(%rbx)
	movq	%rax, 88(%rbx)
	movq	%r13, 96(%rbx)
	movq	%r9, 104(%rbx)
	movq	%rdi, 112(%rbx)
	movq	%r14, 120(%rbx)
	movq	%rsi, 128(%rbx)
	movq	%rcx, 136(%rbx)
	movq	-120(%rsp), %rax        # 8-byte Reload
	movq	%rax, 144(%rbx)
	movq	%r15, 152(%rbx)
	movq	248(%rsp), %rax         # 8-byte Reload
	movq	%rax, 16(%rbx)
	movq	-80(%rsp), %rax         # 8-byte Reload
	movq	%rax, 160(%rbx)
	movq	88(%rsp), %rax          # 8-byte Reload
	movq	%rax, 168(%rbx)
	movq	-104(%rsp), %rax        # 8-byte Reload
	movq	%rax, 176(%rbx)
	movq	%r10, 184(%rbx)
	movq	-64(%rsp), %rax         # 8-byte Reload
	movq	%rax, 192(%rbx)
	movq	-112(%rsp), %rax        # 8-byte Reload
	movq	%rax, 200(%rbx)
	movq	%r8, 208(%rbx)
	movq	208(%rsp), %rax         # 8-byte Reload
	movq	%rax, 216(%rbx)
	movq	-16(%rsp), %rax         # 8-byte Reload
	movq	%rax, 224(%rbx)
	movq	-48(%rsp), %rax         # 8-byte Reload
	movq	%rax, 24(%rbx)
	movq	-24(%rsp), %rax         # 8-byte Reload
	movq	%rax, 32(%rbx)
	movq	%rdx, 40(%rbx)
	movq	-32(%rsp), %rax         # 8-byte Reload
	movq	%rax, 48(%rbx)
	movq	176(%rsp), %rax         # 8-byte Reload
	movq	%rax, 56(%rbx)
	movq	%r12, 64(%rbx)
	movq	(%rsp), %rax            # 8-byte Reload
	movq	%rax, 72(%rbx)
	addq	$432, %rsp              # imm = 0x1B0
	.cfi_def_cfa_offset 56
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
	.size g, .Lfunc_end0-g
	.cfi_endproc
                                        # -- End function
	.ident	"clang version 10.0.0-4ubuntu1 "
	.section	".note.GNU-stack","",@progbits
