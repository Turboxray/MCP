;/////////////////////////////////////////////////////////////////////////////// 
;
; PCE 8k Compo 2010 
;  
;  Build 1.0.0
;
;
	
  .list
  .mlist

	include "equ.asm"
	include "macro.asm"
  
START_BANK .EQU 0
CODE_BANK  .EQU START_BANK+$01  
DATA_BANK  .EQU START_BANK+$10



  .bank START_BANK             
    include "_startup.asm"
    .code
  



;*******************************************************************************
;///////////////////////////////////////////////////////////////////////////////*
;//MAIN                                                                       //*
;///////////////////////////////////////////////////////////////////////////////*
;.
main:                             

      jsr init_vdc
      jsr init_wsg

      VCE_REG 	LO_RES|H_FILTER
      VDC_REG 	DCR , AUTO_SATB_ON          
      VDC_REG 	CR ,  $0000
      VDC_REG 	RCR , $0000
      IRQ_CNTR 	IRQ2_OFF|VIRQ_ON|TIRQ_OFF
      VDC_REG 	SATB , $7F00
      VDC_REG 	MWR , SCR32_64
      TIMER_REG TMR_CMD , #$00
      TIMER_REG TMR_PORT , #$00



      VDC_REG 	CR , BG_ON|SPR_ON|VINT_ON|HINT_ON

      
    ;load font
      VDC_REG 	MAWR, $1000
      VDC_REG 	VRWR
      ;DMA 			Font, $6000, vdata_port, (FontEnd-Font)
    
    ;load palette
      BG_COLOR 	#$0
      ;DMA_local FontPal,vce_data, #$10
      
     

     
      cli 
      
main_loop:

			lda #$00
			sta LZDEST
			lda #$24
			sta LZDEST+1
			ldy #low(BGTile)
			ldx #high(BGTile)
		jsr pu_decmp

			VDC_REG MAWR , $1000
			VDC_REG VRWR
		jsr DecodeTile1



			VDC_REG MAWR , $1800
			VDC_REG VRWR
		jsr DecodeTile2


			lda #$00
			sta LZDEST
			lda #$24
			sta LZDEST+1
			ldy #low(BGMap)
			ldx #high(BGMap)
		jsr pu_decmp

			VDC_REG MAWR , $0040
			VDC_REG VRWR			
			;tia $2400, $0002, ($77f)
		jsr DecodeMap1
		jsr DecodeMap2
					
			
			
			
			VDC_REG	BXR , $00f8
			VDC_REG BYR , $0008
			
			stz $402
			stz $403
			ldx #$10
.lp03
			tia BGPal, $404, $20
			dex
			bne .lp03
			
			clx
			cly
			lda #$02
			sta <A0
			stz <R0
			stz <R1
			lda #$24
			sta <R0+1
			inc a
			sta <R1+1
			cla
			stz $402
			stz $403
.make_loop
			sta [R0],y
			sta $404
			sax
			sta [R1],y
			sta $405
			sax
			inc a
		bne .skip
			inx 
.skip
			iny
			;bne .make_loop
			;dec <A0
		beq .out
			;inc <R0+1
		bra .make_loop						
.out

.pause
			;bra .pause

			cly			
.loop2
			lda #$01
			sta __vblank
.wait
			lda __vblank
		bne .wait
			iny
			ldx #$00
			lda #$01
			sta $402
			stz $403
			lda #$08
			sta <A0
			
.copy_loop
			lda $2400,y
			sta $404
			lda $2500,y
			sta $405
			iny
			dec <A0
		bne .pad
			lda #$08
			sta <A0
			tia BGPal+$12,$404,$10
			
;			phx
;			ldx #$08
;.padlp
;			stz $404			
;			stz $405
;			dex
;		bne .padlp
;			plx

.pad			
			dex
		bne .copy_loop
		bra .loop2
			
			
			
.Phase4loop
		jmp .Phase4loop


;...............................................................................
;End MAIN



;...............................................................................
;///////////////////////////////////////////////////////////////////////////////.
;///////////////////////////////////////////////////////////////////////////////.
;//Main library																															  //.
;///////////////////////////////////////////////////////////////////////////////.
;.

	.include "pce_decmp.asm"
	.include "init_hw.asm"
	
EndLib:
;...............................................................................
;End MAIN



;...............................................................................
;///////////////////////////////////////////////////////////////////////////////.
;///////////////////////////////////////////////////////////////////////////////.
;//Subroutines.																															  //.
;///////////////////////////////////////////////////////////////////////////////.
;.

;//-----
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


;...............................................................................
;End SUB




;...............................................................................
;///////////////////////////////////////////////////////////////////////////////.
;///////////////////////////////////////////////////////////////////////////////.
;//Main Library data	(near)																								  //.
;///////////////////////////////////////////////////////////////////////////////.
;.
	
SprLogoLayer:
	incbin	"logo_lo.lzss"
SprLogoLayerEnd:

BGTile:
	incbin "tile.lzss"
BGTileEnd:

BGMap:
	incbin "map.lzss"
BGMAPEnd:

BGPal:
	incbin "pce.pal"
BGPalEnd:

EndMain:
;...............................................................................
;End Lib Data


	
;...............................................................................
;///////////////////////////////////////////////////////////////////////////////.
;///////////////////////////////////////////////////////////////////////////////.
;//Interrupt handlers.																											  //.
;///////////////////////////////////////////////////////////////////////////////.
;.

	.bank START_BANK
	.org EndMain
	.code
	
;#int
IRQ_2:

  	rti
;#end int


;#int
TIMER:
			stz $1403					
	rti
;#end int


;#int
NMI:

  rti
;#end int


;#int
VDC_INT:
			pha
			lda vdc_status
			and #$20
		bne .vsync
		bra .hsync
	
.vsync
			;lda vdc_reg
			;sta $0000
			stz __vblank
		bra .exit 

.hsync
			;// nothing to do	
	
.exit	
			pla
	rti
;#end int

;...............................................................................
;End INT
	


;...............................................................................
;///////////////////////////////////////////////////////////////////////////////.
;///////////////////////////////////////////////////////////////////////////////.
;//Interrupt Vector Table																										  //.
;///////////////////////////////////////////////////////////////////////////////.
;.
	.bank START_BANK
	.code

		.org $FFF6    ;reset vector pointing to _startup
		.dw IRQ_2
		.dw VDC_INT
	  .dw TIMER 
	  .dw NMI
	  .dw _startup
;...............................................................................
;End VECTOR


;...............................................................................
;*******************************************************************************
;*//MAIN BANK END																													     *
;*******************************************************************************
  




;...............................................................................
;//Start of far/near subroutine and data banks																											     

	


;...............................................................................
;///////////////////////////////////////////////////////////////////////////////.
;///////////////////////////////////////////////////////////////////////////////.
;//Data	(far/near)																													  //.
;///////////////////////////////////////////////////////////////////////////////.
;.
		
	
;...............................................................................
;End DATA


;...............................................................................
;///////////////////////////////////////////////////////////////////////////////.
;///////////////////////////////////////////////////////////////////////////////.
;//Variables																																  //.
;///////////////////////////////////////////////////////////////////////////////.
;.

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
			
			
			

		

;...............................................................................
;///////////////////////////////////////////////////////////////////////////////.
;///////////////////////////////////////////////////////////////////////////////.
;//Equates  																																  //.
;///////////////////////////////////////////////////////////////////////////////.
;.

		;here
			
;...............................................................................
;End LOCAL
				
				
				
				
;...............................................................................
;///////////////////////////////////////////////////////////////////////////////.
;///////////////////////////////////////////////////////////////////////////////.
;//Notes																																		  //.
;///////////////////////////////////////////////////////////////////////////////.
;.

;		Memory layout
;
;				0 $0000 ff:hardware				Fixed 
;				1 $2000 f8:ram						Fixed  
;				2 $4000 sub code/data			Loose / far calls and far tables 
;				3 $6000 far data					Loose / data 
;				4 $8000 far data					Loose / data 
;				5 $a000 far data					Loose / data 
;				6 $c000 sub code					Semi  / Sub code, lib, small near data
;				7 $e000 main code 				Semi  / Main code and small near data






	