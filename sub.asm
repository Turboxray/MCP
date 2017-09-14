
	.org $3c00
	.code
DecodeTile1:
			lda #$24
			stz <A0
			sta <A0+1
			lda #97
			sta <R2
			lda #$04
			sta <R3
			
			cly
			clx
.lp00
			lda [A0],y
			lsr a
			lsr a
			lsr a
			lsr a
			
			lsr a
			rol TileBuffer+0,x
			lsr a
			rol TileBuffer+1,x
			lsr a
			rol TileBuffer+16,x
			lsr a
			rol TileBuffer+17,x

			lda [A0],y
			lsr a
			rol TileBuffer+0,x
			lsr a
			rol TileBuffer+1,x
			lsr a
			rol TileBuffer+16,x
			lsr a
			rol TileBuffer+17,x
			iny
			dec <R3
			bne .lp00
			lda #$04
			sta <R3
			inx
			inx
			cpx #$10
			bcc .lp00
			tia TileBuffer,$0002,$20
			cly
			clx
			lda <A0
			clc
			adc #$20
			sta <A0
			lda <A0+1
			adc #$00
			sta <A0+1
			dec <R2
			bne .lp00
	rts
;end

;//-----
DecodeTile2:
			lda #$24
			stz <A0
			sta <A0+1
			lda #97
			sta <R2
			lda #$04
			sta <R3
			
			cly
			clx
.lp01
			lda [A0],y
			lsr a
			lsr a
			lsr a
			lsr a
			
			cmp #$09
			bcc .zero
			;sec
			;sbc #$08
			bra .cont
.zero
			cla
.cont			
			lsr a
			rol TileBuffer+0,x
			lsr a
			rol TileBuffer+1,x
			lsr a
			rol TileBuffer+16,x
			lsr a
			rol TileBuffer+17,x

			lda [A0],y
			and #$0f
			cmp #$09
			bcc .zero1
			;sec
			;sbc #$08
			bra .cont1
.zero1
			cla
.cont1			
			lsr a
			rol TileBuffer+0,x
			lsr a
			rol TileBuffer+1,x
			lsr a
			rol TileBuffer+16,x
			lsr a
			rol TileBuffer+17,x
			iny
			dec <R3
			bne .lp01
			lda #$04
			sta <R3
			inx
			inx
			cpx #$10
			bcc .lp01
			tia TileBuffer,$0002,$20
			cly
			clx
			lda <A0
			clc
			adc #$20
			sta <A0
			lda <A0+1
			adc #$00
			sta <A0+1
			dec <R2
			bne .lp01
	rts
;end

;//-----
DecodeMap1:
			lda #$24
			stz <A0
			sta <A0+1
			stz <R0
			ldx #$10
			lda #28
			sta <R1
			cly
.lp02	
			lda [A0],y
			iny
			sta $0002
			lda [A0],y
			iny
			and #$0f
			ora <R0
			sta $0003
			
			lda [A0],y
			iny
			sta $0002
			lda [A0],y
			iny
			and #$0f
			ora <R0
			sta $0003
			
			lda <R0
			clc
			adc #$10
			sta <R0
			dex
			bne .lp02
			tya
			clc
			adc A0
			sta A0
			lda A0+1
			adc #$00
			sta A0+1
			cly
			ldx #$10
			stz <R0
			dec <R1
			bne .lp02
	rts
;end

DecodeMap2:
			lda #$24
			stz <A0
			sta <A0+1
			ldx #$10
			lda #28
			sta <R1
			cly
.lp02	
			lda [A0],y
			iny
			clc
			adc #$80
			sta $0002
			lda [A0],y
			iny
			adc #$00
			and #$0f
			ora #$f0
			sta $0003
			
			lda [A0],y
			iny
			clc
			adc #$80
			sta $0002
			lda [A0],y
			iny
			adc #$00
			and #$0f
			ora #$f0
			sta $0003
			
			dex
			bne .lp02
			tya
			clc
			adc A0
			sta A0
			lda A0+1
			adc #$00
			sta A0+1
			cly
			ldx #$10
			dec <R1
			bne .lp02
	rts
;end




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
			

	.bss
			
			__DMA:					.ds 8
			TileBuffer:			.ds 32
			
			
			
			EndBss:					.ds 1
			
			
	