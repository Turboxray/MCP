VDC1	.macro			;hardware I/O page must be mapped to the first bank

	stz $000E

	.endm


VDC2	.macro			;hardware I/O page must be mapped to the first bank

	inc $000E

	.endm

MAWR_ADDR .macro
	st0 #$00
	st1 #LOw(\1)
	st2 #HIGH(\1)

	.endm

MARR_ADDR .macro
	st0 #$01
	st1 #LOw(\1)
	st2 #HIGH(\1)

	.endm

VDC_DATA .macro
	st0 #$02

	.endm

VDC_REG	 .macro

	.if	(\?2=1)
	st0 #\1
	sta $0002
	stz $0003
	.endif
	
	.if	(\#=1)
	st0 #\1
	.endif

	.if	(\#=2 & \?2 != 1)
	st0 #\1
	st1 #LOW(\2)	
	st2 #HIGH(\2)
	 .endif

	.if	(\#=3 & \?3 != 1)
	st0 #\1
	lda \2
	sta $0002
	lda \3
	sta $0003
	.endif

	.endm

VDC2_REG .macro				;macro for SuperGrafx

	.if	(\?2=1)
	st0 #\1
	sta $0012
	stz $0013
	.endif
	
	.if	(\#=1)
	st0 #\1
	.endif

	.if	(\#=2 & \?2 != 1)
	st0 #\1
	st1 #LOW(\2)	
	st2 #HIGH(\2)
	 .endif

	.if	(\#=3 & \?3 != 1)
	st0 #\1
	lda \2
	sta $0012
	lda \3
	sta $0013
	.endif

	.endm

LOAD_RCR .macro

	st0 #RCR	
	lda \1
	clc
	adc #$40
	sta $0002
	lda \1+1
	adc #$00
	sta $0003

	.endm

UPDATE_RCR .macro		;this is for special H-line parallax scroll routine
				; - destroys REG A
	st0 #RCR
	sta $0002
	lda <RCR_MSB
	sta $0003

	 .endm

WRT_PORT .macro

	 st1 #LOW(\1)	
	 st2 #HIGH(\1)

	.endm

STWYA_PORT .macro

	 sta $0002
	 sty $0003

	.endm

STWYA_PORT_2 .macro

	 sta $0012
	 sty $0013

	.endm


BG_COLOR .macro

	lda #(\1)
	sta $402
	stz $403

	.endm

VCE_REG .macro

	lda #(\1)
	sta $400

	.endm

CLEAR_REGS .macro

	cla
	cly
	clx

	.endm

PUSH_R .macro
	
	pha
	phy
	phx

	.endm

PULL_R .macro

	plx
	ply
	pla

	.endm

CALL .macro

	jsr \1

	.endm

INC_BIT .macro

	lda \1
	inc a
	and #$01
	sta \1

	.endm

IRQ_CNTR	 .macro

	lda #\1
	sta $1402
	
	.endm

VREG_Select .macro

	st0 #\1
	lda #\1
	sta <vdc_reg
	
	.endm


sVDC_REG	 .macro

	.if	(\?2=1)
	lda #\1
	sta <vdc_reg
	st0 #\1
	sta $0002
	stz $0003
	.endif
	
	.if	(\#=1)
	lda #\1
	sta <vdc_reg
	st0 #\1
	.endif

	.if	(\#=2 & \?2 != 1)
	lda #\1
	sta <vdc_reg
	st0 #\1
	st1 #LOW(\2)	
	st2 #HIGH(\2)
	 .endif

	.if	(\#=3 & \?3 != 1)
	lda #\1
	sta <vdc_reg
	st0 #\1
	lda \2
	sta $0002
	lda \3
	sta $0003
	.endif

	.endm

iVDC_PORT	 .macro

	st1 #LOW(\1)	
	st2 #HIGH(\1)

	.endm

sVDC_INC	 .macro

	lda #$05
	sta <vdc_reg
	st0 #$05
	st2 #\1

	.endm
	
TIMER_REG		.macro
		lda #\2
		sta \1
	.endm


MAP_BANK		.macro				;8k

	lda #bank(\1)
	tam #(\2)
	
	.endm
	
MAP_BANK_WIDE		.macro		;16k
	
	.if (\?1=6)
	lda #bank(\1)
	tam #(\2)
	inc a
	tam #(\2+1)
	.endif
	
	.if (\?1=3)
	lda \1
	tam #(\2)
	inc a
	tam #(\2+1)
	.endif

	.if (\?1=1)
	tam #(\2)
	inc a
	tam #(\2+1)
	.endif
	
	.endm

MAP_BANK_LONG		.macro		;24k

	lda #bank(\1)
	tam #(\2)
	inc a
	tam #(\2+1)
	inc a
	tam #(\2+2)
	
	.endm

MAP_BANK_XLONG		.macro	;32k

	lda #bank(\1)
	tam #(\2)
	inc a
	tam #(\2+1)
	inc a
	tam #(\2+2)
	inc a
	tam #(\2+3)
	
	.endm


LEA		.macro

	lda #low(\1)
	sta <(\3)
	lda #high((\1 & $1fff)+ \2)
	sta <(\3+1)

	.endm	

LEA_l			.macro

	lda #low(\1)
	sta (\3)
	lda #high((\1 & $1fff)+ \2)
	sta (\3+1)
	lda #bank(\1)
	sta (\3+2)
	.endm	


LEB			.macro

	lda #bank(\1)
	sta (\2)

	.endm	


INCW		.macro
			inc \1
			bne .x_\@
			inc \1+1
.x_\@
	.endm

ADDQW		.macro
	inc (\1)
	bne .x_\@
	inc (\1+1)
.x_\@

	.endm

MOVEA_r		.macro
		lda (\1)
		sta <(\2)
		lda (\1+1)
		sta <(\2+1)
	.endm

MOVEA_r_l		.macro
		lda \1
		sta \2
		lda \1+1
		sta \2+1
		lda \1+2
		sta \2+2
	.endm
	
MOVEB		.macro
	.if (\?1=4)
	
		lda \1
		sta \2
	.endif
	
	.if (\?1=3)
		lda \1
		sta \2
	.endif
	
	.if (\?1=2)
		lda \1
		sta \2
	.endif

	.if (\?1=6)
		lda \1
		sta \2
	.endif
		
	.endm

MOVRX		.macro
	.if (\?1=4)
	
		ldx \1
		stx \2
	.endif
	
	.if (\?1=3)
		ldx \1
		stx \2
	.endif
	
	.if (\?1=2)
		ldx \1
		stx \2
	.endif

	.if (\?1=6)
		ldx \1
		stx \2
	.endif

	.if (\?1=1)
		stx \2
	.endif
	
		
	.endm

MOVRA		.macro
	.if (\?1=4)
	
		lda \1
		sta \2
	.endif
	
	.if (\?1=3)
		lda \1
		sta \2
	.endif
	
	.if (\?1=2)
		lda \1
		sta \2
	.endif

	.if (\?1=6)
		lda \1
		sta \2
	.endif

	.if (\?1=1)
		sta \2
	.endif

	.endm

MOVRY		.macro
	.if (\?1=4)
	
		ldy \1
		sty \2
	.endif
	
	.if (\?1=3)
		ldy \1
		sty \2
	.endif
	
	.if (\?1=2)
		ldy #\1
		sty \2
	.endif

	.if (\?1=6)
		ldy \1
		sty \2
	.endif

	.if (\?1=1)
		sty \2
	.endif
	
		
	.endm


MOVEW		.macro
	.if (\?1=4)
	
		lda \1
		sta \2
		ldy #1
		lda \1,y
		sta \2+1
	.endif
	
	.if (\?1=3)
		lda \1
		sta \2
		lda \1+1
		sta \2+1
	.endif
	
	.if (\?1=2)
		lda #low(\1)
		sta \2
		lda #high(\1)
		sta \2+1
	.endif

	.if (\?1=6)
		lda #low(\1)
		sta \2
		lda #high(\1+1)
		sta \2+1
	.endif
		
	.endm


MOVIA_l		.macro

		ldx #\7-1
.x_\@
		lda \1,x
		sta \4,x
		lda \2,x
		sta \5,x
		lda \3,x
		sta \6,x
		dex
		bpl .x_\@
	.endm

MOVI_l		.macro

		lda \1
		sta \2
		lda \1+1
		sta \3
		lda \1+2
		sta \4
	.endm		



DMA		.macro
		tia (\1 & $1fff)+\2,\3,\4
	.endm
	
DMA_local		.macro
		tia \1,\2,\3
	.endm

PRINT_STR		.macro
      lda #low(\1)
      sta <R0
      lda #high(\1)
      sta <R0+1
      lda #low((\2 & $3f)+((\3)<<6))
      ldx #low((\3 & $3f)>>2)
			clc
      jsr PrintString
	.endm

PRINT_CHN_NOTE	.macro
			ldy #\1      
      lda #low((\2 & $3f)+((\3)<<6))
      ldx #low((\3 & $3f)>>2)
			clc
      jsr PrintChannelNote
	.endm


PRINT_STR_q		.macro
      lda #low((\1 & $3f)+((\2)<<6))
      ldx #low((\2 & $3f)>>2)
			clc
      jsr PrintString
	.endm

PRINT_STR_s		.macro
			lda \1
			and #$3f
			sta <D7
			lda \2
			asl a
			asl a
			asl a
			asl a
			asl a
			asl a
			clc
			adc <D7
			sax
			lda \2
			lsr a
			lsr a
			sax
			clc
      jsr PrintString
	.endm

PRINT_CHAR_s		.macro
			lda \1
			and #$3f
			sta <D7
			lda \2
			asl a
			asl a
			asl a
			asl a
			asl a
			asl a
			clc
			adc <D7
			sax
			lda \2
			lsr a
			lsr a
			sax
			clc
      jsr PrintByte
	.endm


PRINT_CHAR_a		.macro
			sec
      jsr PrintByte
	.endm

PRINT_CHAR_a_q		.macro
			ldy \1
			sec
      jsr PrintByte
	.endm


PRINT_STR_i   .macro
      bra .y_\@
.x_\@:
      .db \1,0
.y_\@:
      lda #low(.x_\@)
      sta <R0
      lda #high(.x_\@)
      sta <R0+1
      lda #low((\2 & $3f)+((\3)<<6))
      ldx #low((\3 & $3f)>>2)
      clc
      jsr PrintString
  .endm


PRINT_STR_i_a   .macro
      bra .y_\@
.x_\@:
      .db \1,0
.y_\@:
      lda #low(.x_\@)
      sta <R0
      lda #high(.x_\@)
      sta <R0+1
      sec
      jsr PrintString
  .endm



ADDIW		.macro
	.if (\?2=4)
	
		lda #low(\1)
		clc
		adc \2
		sta \2
		ldy #1
		lda #high(\1)
		adc \2,y
		sta \2,y
	.endif
	
	.if (\?2=3)
		lda #low(\1)
		clc
		adc \2
		sta \2
		lda #high(\1)
		adc \2+1
		sta \2+1
	.endif

	.if (\?2=6)
		lda #low(\1)
		clc
		adc \2
		sta \2
		lda #high(\1)
		adc \2+1
		sta \2+1
	.endif
	
	.if (\#=1)
		clc
		adc #low(\1)
		sax
		adc #high(\1+1)
		sax
	.endif
		
	.endm

ADDIB		.macro
	.if (\?2=4)
	
		lda #low(\1)
		clc
		adc \2
		sta \2
	.endif
	
	.if (\?2=3)
		lda #low(\1)
		clc
		adc \2
		sta \2
	.endif

	.if (\?2=6)
		lda #low(\1)
		clc
		adc \2
		sta \2	
	.endif
	
	.if (\#=1)
		clc
		adc #low(\1)
		sta \2
	.endif
		
	.endm


WAITVBLANK		.macro
		ldx #\1+1
		jsr __wait_vblank
	.endm





	
bmp	.macro								; <- test macro. Basically a struct define
	\1.size_x: .rs 2
	\1.size_y: .rs 2
	.endm



