;/////////////////////////////////////////////////////////////////////////////// 
; Pucrunch decompressor 
;  
;  Build 1.3.0
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
      jsr init_dma

      VCE_REG 	LO_RES|H_FILTER
      VDC_REG 	DCR , AUTO_SATB_ON          
      VDC_REG 	CR ,  $0000
      VDC_REG 	RCR , $0000
      IRQ_CNTR 	IRQ2_OFF|VIRQ_ON|TIRQ_OFF
      VDC_REG 	SATB , $7F00
      VDC_REG 	MWR , SCR32_32
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
      
      jsr ClearScreen
     
     lda #bank(bg_map)
     tam #$02
     inc a
     tam #$03
     inc a
     tam #$04
     inc a
     tam #$05
     st0 #$00
     st1 #$00
     st2 #$00
     st0 #$02
   	 tia (bg_map & $1fff)+$4000,$0002,(32*2*28)
		
     lda #bank(bg_tle)
     tam #$02
     inc a
     tam #$03
     inc a
     tam #$04
     inc a
     tam #$05
     st0 #$00
     st1 #$00
     st2 #$10
     st0 #$02
   	 tia (bg_tle & $1fff)+$4000,$0002,(896*32)


		 lda #bank(bg_pal)
		 tam #$02
		 inc a
		 tam #$03
		 stz $402
		 stz $403
   	 tia (bg_pal & $1fff)+$4000, $404, $200 
     
     ;1C00
     lda #bank(lunar)
     tam #$02
     inc a
     tam #$03
     inc a
     tam #$04
     inc a
     tam #$05
     st0 #$00
     st1 #$00
     st2 #$50
     st0 #$02
   	 tia (lunar & $1fff)+$4000,$0002,$1c00
 
 		 lda #bank(lunar_pal)
		 tam #$02
		 inc a
		 tam #$03
		 stz $402
		 lda #$01
		 sta $403
   	 tia (lunar_pal & $1fff)+$4000, $404, $20 
     

     lda #bank(sat)
     tam #$02
     inc a
     tam #$03
     inc a
     tam #$04
     inc a
     tam #$05
     st0 #$00
     st1 #$00
     st2 #$7f
     st0 #$02
   	 tia (sat & $1fff)+$4000,$0002,$90


     lda #bank(subtitle)
     tam #$02
     inc a
     tam #$03
     inc a
     tam #$04
     inc a
     tam #$05
     st0 #$00
     st1 #$00
     st2 #$5e
     st0 #$02
   	 tia (subtitle & $1fff)+$4000,$0002,$500

     lda #bank(option)
     tam #$02
     inc a
     tam #$03
     inc a
     tam #$04
     inc a
     tam #$05
     st0 #$02
   	 tia (option & $1fff)+$4000,$0002,$500

 		 lda #bank(subtitle_pal)
		 tam #$02
		 inc a
		 tam #$03
		 lda #$10
		 sta $402
		 lda #$01
		 sta $403
   	 tia (subtitle_pal & $1fff)+$4000+$1e0, $404, $20 

 		 lda #bank(option_pal)
		 tam #$02
		 inc a
		 tam #$03
		 lda #$20
		 sta $402
		 lda #$01
		 sta $403
   	 tia (option_pal & $1fff)+$4000+$1e0, $404, $20 

     
     
     lda #$00
     sta SCR3
     lda #$00
     sta SCR4
     
     lda #48
     sta <SrcWidth
     lda #64
     sta <SrcHeight
     lda #255
     sta <ScaleX
     sta <ScaleY

			jsr Scale_Image
			lda #$02
			sta vdc_reg
			st0 #$02
			tia CellBffr,$0002,(3*4*128)

;// convert the sprites from planar to 8bit

     
      cli 
      
main_loop:
			WAITVBLANK 0
			
			
    jmp main_loop

;...............................................................................
;End MAIN



;...............................................................................
;///////////////////////////////////////////////////////////////////////////////.
;///////////////////////////////////////////////////////////////////////////////.
;//Main library																															  //.
;///////////////////////////////////////////////////////////////////////////////.
;.

	.include "main_func.asm"
	.include "lz_decmp.asm"
	.include "init_hw.asm"
	.include "gamepad.asm"	
	
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
ScrollGrass:
			
			lda SATBB+2
			clc
			adc #$01
			sta SATBB+2
			lda SATBB+1+2
			adc #$00
			sta SATBB+1+2

			lda SATBB+8+2
			clc
			adc #$01
			sta SATBB+8+2
			lda SATBB+8+1+2
			adc #$00
			sta SATBB+8+1+2

			lda SATBB+$10+2
			clc
			adc #$01
			sta SATBB+$10+2
			lda SATBB+$10+1+2
			adc #$00
			sta SATBB+$10+1+2

			lda SATBB+$18+2
			clc
			adc #$01
			sta SATBB+$18+2
			lda SATBB+$18+1+2
			adc #$00
			sta SATBB+$18+1+2

			lda SATBB+$20+2
			clc
			adc #$01
			sta SATBB+$20+2
			lda SATBB+$20+1+2
			adc #$00
			sta SATBB+$20+1+2
			
		rts


;//-----
ScrollLunar:

			lda SATBB+$28+2
			clc
			adc #$01
			sta SATBB+$28+2
			lda SATBB+$28+1+2
			adc #$00
			sta SATBB+$28+1+2
			
		rts

;//-----
ScrollMoon:

			lda SATBB+$30+2
			clc
			adc #$01
			sta SATBB+$30+2
			lda SATBB+$30+1+2
			adc #$00
			sta SATBB+$30+1+2

			lda SATBB+$38+2
			clc
			adc #$01
			sta SATBB+$38+2
			lda SATBB+$38+1+2
			adc #$00
			sta SATBB+$38+1+2

			lda SATBB+$40+2
			clc
			adc #$01
			sta SATBB+$40+2
			lda SATBB+$40+1+2
			adc #$00
			sta SATBB+$40+1+2

			lda SATBB+$48+2
			clc
			adc #$01
			sta SATBB+$48+2
			lda SATBB+$48+1+2
			adc #$00
			sta SATBB+$48+1+2

			lda SATBB+$50+2
			clc
			adc #$01
			sta SATBB+$50+2
			lda SATBB+$50+1+2
			adc #$00
			sta SATBB+$50+1+2

			lda SATBB+$58+2
			clc
			adc #$01
			sta SATBB+$58+2
			lda SATBB+$58+1+2
			adc #$00
			sta SATBB+$58+1+2

			lda SATBB+$60+2
			clc
			adc #$01
			sta SATBB+$60+2
			lda SATBB+$60+1+2
			adc #$00
			sta SATBB+$60+1+2

			lda SATBB+$68+2
			clc
			adc #$01
			sta SATBB+$68+2
			lda SATBB+$68+1+2
			adc #$00
			sta SATBB+$68+1+2

			lda SATBB+$70+2
			clc
			adc #$01
			sta SATBB+$70+2
			lda SATBB+$70+1+2
			adc #$00
			sta SATBB+$70+1+2

			lda SATBB+$78+2
			clc
			adc #$01
			sta SATBB+$78+2
			lda SATBB+$78+1+2
			adc #$00
			sta SATBB+$78+1+2

			lda SATBB+$80+2
			clc
			adc #$01
			sta SATBB+$80+2
			lda SATBB+$80+1+2
			adc #$00
			sta SATBB+$80+1+2

			lda SATBB+$88+2
			clc
			adc #$01
			sta SATBB+$88+2
			lda SATBB+$88+1+2
			adc #$00
			sta SATBB+$88+1+2

			lda SATBB+$90+2
			clc
			adc #$01
			sta SATBB+$90+2
			lda SATBB+$90+1+2
			adc #$00
			sta SATBB+$90+1+2

			lda SATBB+$98+2
			clc
			adc #$01
			sta SATBB+$98+2
			lda SATBB+$98+1+2
			adc #$00
			sta SATBB+$98+1+2

			lda SATBB+$a0+2
			clc
			adc #$01
			sta SATBB+$a0+2
			lda SATBB+$a0+1+2
			adc #$00
			sta SATBB+$a0+1+2

			lda SATBB+$a8+2
			clc
			adc #$01
			sta SATBB+$a8+2
			lda SATBB+$a8+1+2
			adc #$00
			sta SATBB+$a8+1+2

			lda SATBB+$b0+2
			clc
			adc #$01
			sta SATBB+$b0+2
			lda SATBB+$b0+1+2
			adc #$00
			sta SATBB+$b0+1+2

			lda SATBB+$b8+2
			clc
			adc #$01
			sta SATBB+$b8+2
			lda SATBB+$b8+1+2
			adc #$00
			sta SATBB+$b8+1+2

			lda SATBB+$c0+2
			clc
			adc #$01
			sta SATBB+$c0+2
			lda SATBB+$c0+1+2
			adc #$00
			sta SATBB+$c0+1+2

			lda SATBB+$c8+2
			clc
			adc #$01
			sta SATBB+$c8+2
			lda SATBB+$c8+1+2
			adc #$00
			sta SATBB+$c8+1+2

			lda SATBB+$d0+2
			clc
			adc #$01
			sta SATBB+$d0+2
			lda SATBB+$d0+1+2
			adc #$00
			sta SATBB+$d0+1+2

			lda SATBB+$d8+2
			clc
			adc #$01
			sta SATBB+$d8+2
			lda SATBB+$d8+1+2
			adc #$00
			sta SATBB+$d8+1+2

		rts


;//-----
Scale_line:

			ldy <SrcWidth
			ldx <SrcWidth
			stz <cntr
.loop_width
			lda <cntr
			clc
			adc <ScaleX
			sta <cntr
			bcc .no_write
			lda [SrcImg]
			sta [DesImg]
			dey									;Y=dest width
			beq .out
			INCW DesImg
.no_write
			INCW SrcImg
			dex
			bne .loop_width
			cla
.loop_pad
			sta [DesImg]
			INCW DesImg
			dey
			bne .loop_pad
.out

		rts


;//-----
Scale_Image:


			lda #low(LunarImg)
			sta <SrcImg
			lda #high(LunarImg)
			and #$1f
			ora #$40
			sta <SrcImg+1
			
			lda #bank(LunarImg)
			tam #$02
			inc a
			tam #$03
			

			ldx <SrcHeight
			ldy <SrcHeight
			stz <cntr2
			lda #low(LineBffr)
			sta <DesImg
			lda #high(LineBffr)
			sta <DesImg+1
.loop
			lda	<cntr2
			clc
			adc <ScaleY
			sta <cntr2
			bcc .skip_line
			phx
			phy
			jsr Scale_line
			ply
			plx
			dex
.skip_line
			dey											;Y=src lines. X=dest lines.
			bne .loop								;X==Y or Y<X			
			cpx #$00
			bne .source_end_early					
												
.convert_cell_row
			jsr Conv_Bffr
		rts
			
.source_end_early							;Pad the rest of the bitmap buffer lines			
			
.loop_y
			cla
			cly
.loop_x
			sta [DesImg],y
			iny
			cpy #48
			bne .loop_x
			dex
			beq .convert_cell_row
			tya
			clc
			adc DesImg
			sta DesImg
			lda DesImg+1
			adc #$00
			sta DesImg+1
			bra .loop_y
			

;//-----
Conv_Bffr:

			lda #low(LineBffr)
			sta <DesImg
			lda #high(LineBffr)
			sta <DesImg+1
			
							
					; Sprite cell conversion table relative to 48x64 source image
					;
					;		_______
					;		|0|4|8|
					; 	-------
					; 	|1|5|9|
					; 	-------
					; 	|2|6|A|
					; 	-------
					; 	|3|7|B|
					;		-------
					;
					; In memory:
					;
					;    *       *       *
					;		|0|1|2|3|4|5|6|7|8|9|A|B|
					;
					

					; Y reg is the cell row/line start point (Y/2*16=row). Row = 3 sprite cells.
	
					; Conversion routine is based on 16pixel shift. Can be further optimized for
					; split 8pixel shift sections.

			clx
			lda #$40
			sta <cntr2
			ldy #$10


.loop0									; First 16 pixels of 48 
			lda [DesImg]
			INCW DesImg
			lsr a
			rol CellBffr,x						;Plane 0
			rol CellBffr+1,x
			lsr a
			rol CellBffr+32,x					;Plane 1
			rol CellBffr+32+1,x
			lsr a
			rol CellBffr+64,x					;Plane 2
			rol CellBffr+64+1,x
			lsr a
			rol CellBffr+96,x					;Plane 3
			rol CellBffr+96+1,x
			dey
		bne .loop0
			ldy #$10

.loop1									; Second 16 pixels of 48	
			lda [DesImg]
			INCW DesImg
			lsr a
			rol CellBffr+(4*128),x
			rol CellBffr+(4*128)+1,x
			lsr a
			rol CellBffr+(4*128)+32,x
			rol CellBffr+(4*128)+32+1,x
			lsr a
			rol CellBffr+(4*128)+64,x
			rol CellBffr+(4*128)+64+1,x
			lsr a
			rol CellBffr+(4*128)+96,x
			rol CellBffr+(4*128)+96+1,x
			dey
		bne .loop1
			ldy #$10

.loop2									; Third 16 pixels of 48	
			lda [DesImg]
			INCW DesImg
			lsr a
			rol CellBffr+(8*128),x
			rol CellBffr+(8*128)+1,x
			lsr a
			rol CellBffr+(8*128)+32,x
			rol CellBffr+(8*128)+32+1,x
			lsr a
			rol CellBffr+(8*128)+64,x
			rol CellBffr+(8*128)+64+1,x
			lsr a
			rol CellBffr+(8*128)+96,x
			rol CellBffr+(8*128)+96+1,x
			dey
		bne .loop2
			ldy #$10
			
			inx
			inx
			dec <cntr2
		beq .out
		jmp	.loop0
		
.out
								
		rts		


;...............................................................................
;End SUB




;...............................................................................
;///////////////////////////////////////////////////////////////////////////////.
;///////////////////////////////////////////////////////////////////////////////.
;//Main Library data	(near)																								  //.
;///////////////////////////////////////////////////////////////////////////////.
;.
	
	


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
			stz $1403					;5
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
			st0 #$07
			lda SCR3
			sta $0002
			lda vdc_reg
			sta $0000
			stz __vblank
		bra .exit 

.hsync
			;// nothing to do	
			st0 #$07
			lda SCR4
			sta $0002
			lda vdc_reg
			sta $0000
	
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
	  .dw TIMER ;<- old default address
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
		
		.BANK 10 ;.bank $10
		.org $0000

bg_map:
	incbin "bg_bat.bin"
bg_tle:
	incbin "bg_tile.bin"
bg_pal:
	incbin "bg_pal.bin"
bg_end:

sat:
	incbin "sat.dat"
sat_end:

lunar:
	incbin "sprite1.spr"
lunar_pal:
	incbin "sprite1.pal"
lunar_end:

subtitle:
	incspr "sprite2.pcx"
subtitle_pal:
	incpal "sprite2.pcx"

option:
	incspr "sprite3.pcx"
option_pal:
	incpal "sprite3.pcx"

LunarImg:
	incbin "lunar.raw"
LunarPal:
	incbin "lunar.9bp"
	
	
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
			DesImg:			.ds 2
			SrcImg:			.ds 2
			ScaleX:			.ds 1
			ScaleY:			.ds 1
			cntr:				.ds 1
			cntr2:			.ds 1
			SrcWidth:		.ds 1
			SrcHeight:	.ds 1

	.bss
			
			__DMA:					.ds 8
			SATBB:					.ds $e0
			SCR1:						.ds 1
			SCR2:						.ds 1
			SCR3:						.ds 1
			SCR4:						.ds 1
			SCR5:						.ds 1
			LineBffr:				.ds (3*4*256)
			CellBffr:				.ds (3*4*128)
			

		

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






	