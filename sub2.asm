
	.code

;//-----
DecmpSine2:

			cly
			clx
.lp
			lda 0000,y
			sta $3f00,x
			inx
			iny
			cpy #$40
		bne .lp
			
			ldy #$40
.lp1
			lda 0000,y
			sta $3f00,x
			inx
			dey
		bne .lp1
		
			cly
.lp2
			lda 0000,y
			sta $3f00,x
			inx
			iny
			cpy #$40
		bne .lp2
			
			ldy #$40
.lp3
			lda 0000,y
			sta $3f00,x
			inx
			dey
		bne .lp3
	rts
.DecmpRLEend

			
;//-----
DrawColumn:

			ldy #$0e
			lda #$01
			sta <R1
			lda #$01
.lp00
			sta $3d00,x
			stz $3d80,x
			inx
			clc
			adc <R1
			cpy #$09
		bne .lp00skip00
			pha
			lda #$ff
			sta <R1
			pla
.lp00skip00
			dey
		bne .lp00
	rts
.DrawColumnEnd		

;//-----
DrawRow:

			ldy #$0e
			lda #$01
			sta <R1
			lda #$01
.lp00
			sta VColor.lo,x
			stz VColor.hi,x
			inx
			clc
			adc <R1
			cpy #$09
		bne .lp00skip00
			pha
			lda #$ff
			sta <R1
			pla
.lp00skip00
			dey
		bne .lp00
	rts
.DrawRowEnd		

;//-----
ClearRowBuffer:
			clx
.clear_cram
			stz VColor.lo,x
			stz VColor.hi,x
			inx
		bne .clear_cram
	rts
.ClearRowBufferEnd


;//-----
ClearCramBuffer:
			clx
.clear_cram
			stz $3d00,x
			inx
		bne .clear_cram
	rts
.ClearCramBufferEnd



;//-----
DmaCram:

			lda #$10
			sta <R1
			lda #$01
			sta <R2
			sta $402
			stz $403
			ldx #$08
			cly		
.loop
			lda $3d00,y
			sta $404
			lda $3d80,y
			sta $405
			iny
			dex
		bne .loop
			lda <R2
			clc
			adc #$10
			sta <R2
			sta $402
			stz $403
			ldx #$08
			dec <R1
		bne .loop
	rts
.DmaCramEnd



	.zp
	


		;R=reg (generic). A=address. D=data. M=Mpr. (General)
			R0:					.ds 2
			R1:					.ds 2
			R2:					.ds 2
			R3:					.ds 2
			R4:					.ds 2
			R5:					.ds 2
			R6:					.ds 2
			R7:					.ds 2
			R8:					.ds 2
			A0:					.ds 2		
			A1:					.ds 2
			A2:					.ds 2
			A3:					.ds 2
			A4:					.ds 2
			A5:					.ds 2
			A6:					.ds 2
			A7:					.ds 2
			A8:					.ds 2
			D0:					.ds 2
			D1:					.ds 2
			D2:					.ds 2
			D3:					.ds 2
			D4:					.ds 2
			D5:					.ds 2
			D6:					.ds 2
			D7:					.ds 2
			D8:					.ds 2
			M0:					.ds 1			;MPR 2
			M1:					.ds 1			;MPR 3
			M2:					.ds 1     ;MPR 4
			M3:					.ds 1			;MPR 5
			M4:					.ds 1			;MPR 6
			M5:					.ds 1			;MPR 7
	


			
		;VDC regs	(Video)
			__BYR:			.ds 2
			__BXR:			.ds 2
			RCR_start:	.ds 2
			__vblank:		.ds 1
			
		;Normal ZP regs
			RCR.lo:			.ds 1
			RCR.hi:			.ds 1
			VColor.ptr:	.ds 1		
			__wBXR:			.ds 1

	.bss
			
			__DMA:					.ds 8
			TileBuffer:			.ds 32
			VColor.lo:			.ds 240
			VColor.hi:			.ds 240
			
			
			
			
			EndBss:					.ds 1
