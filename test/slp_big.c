void f(long* A, long* B, long* C){ 
	long c0;
	long c1;
	long c10;
	long c11;
	long c12;
	long c13;
	long c14;
	long c15;
	long c16;
	long c17;
	long c18;
	long c19;
	long c2;
	long c20;
	long c21;
	long c22;
	long c23;
	long c24;
	long c25;
	long c26;
	long c27;
	long c28;
	long c3;
	long c4;
	long c5;
	long c6;
	long c7;
	long c8;
	long c9;
	long a0 = a[0];
	long a1 = a[1];
	long a10 = a[10];
	long a11 = a[11];
	long a12 = a[12];
	long a13 = a[13];
	long a14 = a[14];
	long a2 = a[2];
	long a3 = a[3];
	long a4 = a[4];
	long a5 = a[5];
	long a6 = a[6];
	long a7 = a[7];
	long a8 = a[8];
	long a9 = a[9];
	long b0 = b[0];
	long b1 = b[1];
	long b10 = b[10];
	long b11 = b[11];
	long b12 = b[12];
	long b13 = b[13];
	long b14 = b[14];
	long b2 = b[2];
	long b3 = b[3];
	long b4 = b[4];
	long b5 = b[5];
	long b6 = b[6];
	long b7 = b[7];
	long b8 = b[8];
	long b9 = b[9];
	long c0 = a0 & b0;
	long t2 = a0 & b1;
	long t3 = a1 & b0;
	long t5 = a0 & b2;
	long t6 = a1 & b1;
	long t7 = a2 & b0;
	long t10 = a1 & b2;
	long t11 = a2 & b1;
	long t13 = a2 & b2;
	long t14 = a3 & b3;
	long t15 = a3 & b4;
	long t16 = a4 & b3;
	long t18 = a3 & b5;
	long t19 = a4 & b4;
	long t20 = a5 & b3;
	long t23 = a4 & b5;
	long t24 = a5 & b4;
	long t26 = a5 & b5;
	long t27 = a6 & b6;
	long t28 = a6 & b7;
	long t29 = a7 & b6;
	long t31 = a6 & b8;
	long t32 = a7 & b7;
	long t33 = a8 & b6;
	long t36 = a7 & b8;
	long t37 = a8 & b7;
	long t39 = a8 & b8;
	long t40 = a9 & b9;
	long t41 = a9 & b10;
	long t42 = a10 & b9;
	long t44 = a9 & b11;
	long t45 = a10 & b10;
	long t46 = a11 & b9;
	long t49 = a10 & b11;
	long t50 = a11 & b10;
	long t52 = a11 & b11;
	long t53 = a12 & b12;
	long t54 = a12 & b13;
	long t55 = a13 & b12;
	long t57 = a12 & b14;
	long t58 = a13 & b13;
	long t59 = a14 & b12;
	long t62 = a13 & b14;
	long t63 = a14 & b13;
	long c28 = a14 & b14;
	long t66 = a3 ^ a0;
	long t67 = a4 ^ a1;
	long t68 = a5 ^ a2;
	long t69 = b3 ^ b0;
	long t70 = b4 ^ b1;
	long t71 = b5 ^ b2;
	long t72 = t66 & t69;
	long t73 = t66 & t70;
	long t74 = t67 & t69;
	long t76 = t66 & t71;
	long t77 = t67 & t70;
	long t78 = t68 & t69;
	long t81 = t67 & t71;
	long t82 = t68 & t70;
	long t84 = t68 & t71;
	long t85 = a6 ^ a0;
	long t86 = a7 ^ a1;
	long t87 = a8 ^ a2;
	long t88 = b6 ^ b0;
	long t89 = b7 ^ b1;
	long t90 = b8 ^ b2;
	long t91 = t85 & t88;
	long t92 = t85 & t89;
	long t93 = t86 & t88;
	long t95 = t85 & t90;
	long t96 = t86 & t89;
	long t97 = t87 & t88;
	long t100 = t86 & t90;
	long t101 = t87 & t89;
	long t103 = t87 & t90;
	long t104 = a12 ^ a6;
	long t105 = a13 ^ a7;
	long t106 = a14 ^ a8;
	long t107 = b12 ^ b6;
	long t108 = b13 ^ b7;
	long t109 = b14 ^ b8;
	long t110 = t104 & t107;
	long t111 = t104 & t108;
	long t112 = t105 & t107;
	long t114 = t104 & t109;
	long t115 = t105 & t108;
	long t116 = t106 & t107;
	long t119 = t105 & t109;
	long t120 = t106 & t108;
	long t122 = t106 & t109;
	long t123 = a12 ^ a9;
	long t124 = a13 ^ a10;
	long t125 = a14 ^ a11;
	long t126 = b12 ^ b9;
	long t127 = b13 ^ b10;
	long t128 = b14 ^ b11;
	long t129 = t123 & t126;
	long t130 = t123 & t127;
	long t131 = t124 & t126;
	long t133 = t123 & t128;
	long t134 = t124 & t127;
	long t135 = t125 & t126;
	long t138 = t124 & t128;
	long t139 = t125 & t127;
	long t140 = t125 & t128;
	long t142 = a9 ^ t85;
	long t143 = a10 ^ t86;
	long t144 = a11 ^ t87;
	long t145 = b9 ^ t88;
	long t146 = b10 ^ t89;
	long t147 = b11 ^ t90;
	long t148 = t142 & t145;
	long t149 = t142 & t146;
	long t150 = t143 & t145;
	long t152 = t142 & t147;
	long t153 = t143 & t146;
	long t154 = t144 & t145;
	long t157 = t143 & t147;
	long t158 = t144 & t146;
	long t160 = t144 & t147;
	long t161 = t104 ^ a3;
	long t162 = t105 ^ a4;
	long t163 = t106 ^ a5;
	long t164 = t107 ^ b3;
	long t165 = t108 ^ b4;
	long t166 = t109 ^ b5;
	long t167 = t161 & t164;
	long t168 = t161 & t165;
	long t169 = t162 & t164;
	long t171 = t161 & t166;
	long t172 = t162 & t165;
	long t173 = t163 & t164;
	long t176 = t162 & t166;
	long t177 = t163 & t165;
	long t179 = t163 & t166;
	long t180 = t123 ^ t66;
	long t181 = t124 ^ t67;
	long t182 = t125 ^ t68;
	long t183 = t126 ^ t69;
	long t184 = t127 ^ t70;
	long t185 = t128 ^ t71;
	long t186 = t180 & t183;
	long t187 = t180 & t184;
	long t188 = t181 & t183;
	long t190 = t180 & t185;
	long t191 = t181 & t184;
	long t192 = t182 & t183;
	long t195 = t181 & t185;
	long t196 = t182 & t184;
	long t198 = t182 & t185;
	long t199 = t180 ^ a6;
	long t200 = t181 ^ a7;
	long t201 = t182 ^ a8;
	long t202 = t183 ^ b6;
	long t203 = t184 ^ b7;
	long t204 = t185 ^ b8;
	long t205 = t199 & t202;
	long t206 = t199 & t203;
	long t207 = t200 & t202;
	long t209 = t199 & t204;
	long t210 = t200 & t203;
	long t211 = t201 & t202;
	long t214 = t200 & t204;
	long t215 = t201 & t203;
	long t217 = t201 & t204;
	long TR1 = t10 ^ t11;
	long TR2 = t100 ^ t101;
	long TR3 = t119 ^ t120;
	long TR4 = t138 ^ t139;
	long TR5 = t157 ^ t158;
	long TR6 = t176 ^ t177;
	long TR7 = t195 ^ t196;
	long TR8 = t214 ^ t215;
	long TR9 = t23 ^ t24;
	long TR10 = t36 ^ t37;
	long TR11 = t49 ^ t50;
	long c27 = t62 ^ t63;
	long TR13 = t81 ^ t82;
	long X0 = TR1 ^ c0;
	long X1 = c27 ^ t53;
	long X2 = TR11 ^ X1;
	long c24 = TR4 ^ X2;
	long X4 = t14 ^ X0;
	long c3 = t72 ^ X4;
	long X6 = TR10 ^ t40;
	long X7 = TR9 ^ t27;
	long X8 = TR13 ^ t91;
	long X9 = X4 ^ X7;
	long c6 = X8 ^ X9;
	long X11 = TR3 ^ t129;
	long X12 = X2 ^ X6;
	long c21 = X11 ^ X12;
	long X14 = TR5 ^ TR8;
	long X15 = t167 ^ t205;
	long X16 = X14 ^ X15;
	long X17 = TR2 ^ t110;
	long X18 = X6 ^ X7;
	long X19 = X17 ^ X18;
	long X20 = TR6 ^ t186;
	long X21 = c24 ^ c6;
	long X22 = X16 ^ X20;
	long c15 = X21 ^ X22;
	long X24 = TR7 ^ t148;
	long X25 = c3 ^ c21;
	long X26 = X16 ^ X24;
	long c12 = X25 ^ X26;
	long X28 = TR1 ^ TR6;
	long X29 = TR7 ^ TR8;
	long X30 = X1 ^ X19;
	long X31 = X28 ^ X29;
	long c18 = X30 ^ X31;
	long X33 = c27 ^ t148;
	long X34 = t186 ^ t205;
	long X35 = X0 ^ X30;
	long X36 = X33 ^ X34;
	long c9 = X35 ^ X36;
	long TR14 = t103 ^ t111;
	long TR15 = t122 ^ t130;
	long TR16 = t149 ^ t150;
	long TR17 = t15 ^ t16;
	long TR18 = t160 ^ t168;
	long TR19 = t187 ^ t188;
	long c1 = t2 ^ t3;
	long TR21 = t206 ^ t207;
	long TR22 = t26 ^ t28;
	long TR23 = t39 ^ t41;
	long TR24 = t54 ^ t55;
	long TR25 = t73 ^ t74;
	long TR26 = t84 ^ t92;
	long X38 = c1 ^ t13;
	long X39 = TR24 ^ c28;
	long X40 = TR17 ^ X38;
	long c4 = TR25 ^ X40;
	long X42 = t52 ^ X39;
	long c25 = t140 ^ X42;
	long X44 = TR22 ^ t29;
	long X45 = TR23 ^ t42;
	long X46 = TR15 ^ t131;
	long X47 = X42 ^ X45;
	long c22 = X46 ^ X47;
	long X49 = TR26 ^ t93;
	long X50 = X40 ^ X44;
	long c7 = X49 ^ X50;
	long X52 = TR18 ^ TR21;
	long X53 = t169 ^ t217;
	long X54 = X52 ^ X53;
	long X55 = TR14 ^ t112;
	long X56 = X44 ^ X45;
	long X57 = X55 ^ X56;
	long X58 = TR16 ^ t198;
	long X59 = c4 ^ c22;
	long X60 = X54 ^ X58;
	long c13 = X59 ^ X60;
	long X62 = TR19 ^ t179;
	long X63 = c25 ^ c7;
	long X64 = X54 ^ X62;
	long c16 = X63 ^ X64;
	long X66 = TR16 ^ TR19;
	long X67 = TR21 ^ TR24;
	long X68 = X38 ^ X57;
	long X69 = X66 ^ X67;
	long c10 = X68 ^ X69;
	long X71 = c1 ^ t179;
	long X72 = t198 ^ t217;
	long X73 = X39 ^ X68;
	long X74 = X71 ^ X72;
	long c19 = X73 ^ X74;
	long TR27 = t114 ^ t115;
	long TR28 = t133 ^ t134;
	long TR29 = t152 ^ t153;
	long TR30 = t171 ^ t172;
	long TR31 = t18 ^ t19;
	long TR32 = t190 ^ t191;
	long TR33 = t209 ^ t210;
	long TR34 = t31 ^ t32;
	long TR35 = t44 ^ t45;
	long TR36 = t5 ^ t6;
	long TR37 = t57 ^ t58;
	long TR38 = t76 ^ t77;
	long TR39 = t95 ^ t96;
	long c2 = TR36 ^ t7;
	long c26 = TR37 ^ t59;
	long X78 = TR31 ^ t20;
	long X79 = c2 ^ X78;
	long X80 = TR34 ^ t33;
	long X81 = TR35 ^ t46;
	long X82 = c26 ^ X81;
	long X83 = TR33 ^ t211;
	long X84 = TR27 ^ t116;
	long X85 = X80 ^ X82;
	long c20 = X84 ^ X85;
	long X87 = TR28 ^ t135;
	long c23 = X82 ^ X87;
	long X89 = TR38 ^ t78;
	long c5 = X79 ^ X89;
	long X91 = TR29 ^ t154;
	long X92 = X83 ^ X91;
	long X93 = TR39 ^ t97;
	long X94 = X79 ^ X80;
	long c8 = X93 ^ X94;
	long X96 = TR30 ^ t173;
	long X97 = TR32 ^ t192;
	long X98 = c2 ^ c20;
	long X99 = X92 ^ X97;
	long c11 = X98 ^ X99;
	long X101 = c23 ^ c5;
	long X102 = X92 ^ X96;
	long c14 = X101 ^ X102;
	long X104 = c26 ^ X83;
	long X105 = c8 ^ X96;
	long X106 = X97 ^ X104;
	long c17 = X105 ^ X106;
	c[0] = c0;
	c[1] = c1;
	c[10] = c10;
	c[11] = c11;
	c[12] = c12;
	c[13] = c13;
	c[14] = c14;
	c[15] = c15;
	c[16] = c16;
	c[17] = c17;
	c[18] = c18;
	c[19] = c19;
	c[2] = c2;
	c[20] = c20;
	c[21] = c21;
	c[22] = c22;
	c[23] = c23;
	c[24] = c24;
	c[25] = c25;
	c[26] = c26;
	c[27] = c27;
	c[28] = c28;
	c[3] = c3;
	c[4] = c4;
	c[5] = c5;
	c[6] = c6;
	c[7] = c7;
	c[8] = c8;
	c[9] = c9;

}
