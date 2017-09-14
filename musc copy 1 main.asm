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



      VDC_REG 	CR , BG_ON|SPR_ON|VINT_OFF|HINT_OFF

      
    ;load font
      VDC_REG 	MAWR, $1000
      VDC_REG 	VRWR
      ;DMA 			Font, $6000, vdata_port, (FontEnd-Font)
    
    ;load palette
      BG_COLOR 	#$0
      ;DMA_local FontPal,vce_data, #$10
      
     

     
      ;cli 
      
main_loop:


			lda #$00
			sta LZDEST
			lda #$3c
			sta LZDEST+1
			ldy #low(sub1)
			ldx #high(sub1)
		jsr pu_decmp


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
					
			
			
			lda #$00
			sta LZDEST
			lda #$3e
			sta LZDEST+1
			ldy #low(Sine)
			ldx #high(Sine)
		jsr pu_decmp
		
		jsr DecmpSine2

			
			VDC_REG	BXR , $00f8
			VDC_REG BYR , $0008
			lda #$f8
			sta <__BXR
			
			
			stz $402
			stz $403
			clx
.clear_cram
			stz $404
			stz $405
			stz $3d00,x
			inx
			cpx #$80
		bcc .clear_cram
			

			ldx #$23
		jsr DrawColumn
		jsr DmaCram
			
			
			lda #$23
			sta <R3
			
      VDC_REG 	CR , BG_ON|SPR_ON|VINT_ON|HINT_ON

			
			lda #$ff
			sta VColor.lo+$40

			stz KeyHold
			tii KeyHold,KeyHold+1,$100
			

			lda #bank(Engine)
			tam #$03
			lda #bank(TestSong)
			tam #$02			
			lda #$01
			sta EngineStat
			clx
			lda #low(SongTrack0)
			sta <TrkPtr,x
			lda #high(SongTrack0)
			sta <TrkPtr+1,x
			inx
			inx
			lda #low(SongTrack1)
			sta <TrkPtr,x
			lda #high(SongTrack1)
			sta <TrkPtr+1,x
			inx
			inx
			lda #low(SongTrack2)
			sta <TrkPtr,x
			lda #high(SongTrack2)
			sta <TrkPtr+1,x
			inx
			inx
			lda #low(SongTrack3)
			sta <TrkPtr,x
			lda #high(SongTrack3)
			sta <TrkPtr+1,x
			inx
			inx
			lda #low(SongTrack4)
			sta <TrkPtr,x
			lda #high(SongTrack4)
			sta <TrkPtr+1,x
			inx
			inx
			lda #low(SongTrack5)
			sta <TrkPtr,x
			lda #high(SongTrack5)
			sta <TrkPtr+1,x
			inx
			inx

			lda #$06
			sta TickLen
			lda #$01
			sta tempo
			lda #$05
			sta TrkCrntWF+0
			sta TrkCrntWF+1
			sta TrkCrntWF+2
			sta TrkCrntWF+3
			sta TrkCrntWF+4
			sta TrkCrntWF+5

			cli

			
			
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.testmusicengine			
			jsr WaitVblank
			jsr Engine
			jmp .testmusicengine	
			
.Phase4loop
		jsr ClearCramBuffer
		jsr ClearRowBuffer
			inc <R6
			inc <R3
			
			lda <R6
			tax
			lda $3f00,x
			clc
			adc #$80
			and #$7f
			tax
		jsr DrawRow

		
			lda #$8
			sta <R4
			lda <R3
			sta <R5
.loop_sine_hori_bars
			lda <R5
			asl a
			tax
			lda $3f00,x
			clc
			adc #$80
			and #$7f
			clc
			;adc #$18
			tax
		jsr DrawColumn
			inc <R5
			inc <R5
			inc <R5
			;inc <R5
			;inc <R5
			;inc <R5
			;inc <R5
			;inc <R5
			dec <R4
		bne .loop_sine_hori_bars

			;inc <R3
			lda #$8
			sta <R4
			lda <R3
			sta <R5
.loop_sine_hori_bars2
			lda <R5
			asl a
			asl a
			clc
			adc #$80
			tax
			lda $3f00,x
			clc
			adc #$80
			and #$7f
			clc
			;adc #$18
			tax
		jsr DrawColumn
			inc <R5
			inc <R5
			;inc <R5
			;inc <R5
			;inc <R5
			;inc <R5
			;inc <R5
			;inc <R5
			dec <R4
		bne .loop_sine_hori_bars2
		
		jsr WaitVblank
		jsr DmaCram

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

sub1:
	.incbin "sub.lzss"
DecodeMap1 = $3cd4
DecodeMap2 = $3d27
DecodeTile1	= $3c00
DecodeTile2	= $3c62

;//-----
DecmpSine2:

			cly
			clx
.lp
			lda Sine2,y
			sta $3f00,x
			inx
			iny
			cpy #$40
		bne .lp
			
			ldy #$40
.lp1
			lda Sine2-1,y
			sta $3f00,x
			inx
			dey
		bne .lp1
		
			cly
.lp2
			lda Sine2+$40,y
			sta $3f00,x
			inx
			iny
			cpy #$40
		bne .lp2
			
			ldy #$40
.lp3
			lda Sine2+$3f,y
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


;//-----
WaitVblank:
			stz <__vblank
			inc <__vblank
.loop
			tst #$01, <__vblank
			bne .loop
	rts
.WaitVblankEnd

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

Sine:
	incbin "sinwave_bg.lzss"

Sine2:
	incbin "sinwave2.dat"

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
			st0 #$06
			st1 #$40
			st2 #$00
			lda <vdc_reg
			sta $0000
			stz <__vblank
			lda #$40
			sta <RCR.lo
			stz <RCR.hi
			stz <VColor.ptr
			lda <__BXR
			sta <__wBXR

		bra .exit 

.hsync
			inc <VColor.ptr
			phy
			ldy <VColor.ptr
			stz $402
			stz $403
			lda VColor.lo,y
			sta $404
			lda VColor.hi,y
			sta $405
			
			lda <RCR.lo
			inc a
			bne	.skip
			inc <RCR.hi
.skip
			sta <RCR.lo		
			st0 #$06
			sta $0002
			lda <RCR.hi
			sta $0003
			
			lda VColor.lo,y
			ora VColor.hi,y
			beq .skip1
			st0 #$08
			st1 #$ff
			st2 #$00
			bra .hsyncend
.skip1
			st0 #$08
			lda <RCR.lo
			sec
			sbc #$40
			sta $0002
			lda <RCR.hi
			sbc #$00
			sta $0003

.hsyncend
			;inc <__wBXR
			;lda <__wBXR
			;st0 #$07
			;sta $0002	
			lda <vdc_reg
			sta $0000
			ply
						
	
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
			RCR.lo:				.ds 1
			RCR.hi:				.ds 1
			VColor.ptr:		.ds 1		
			__wBXR:				.ds 1
			TrkPeriod.lo:	.ds 6
			TrkPeriod.hi:	.ds 6
			TrkPtr:				.ds 6*2
			TrkNum:				.ds 1
			TrkAttrib:		.ds 6
			TrkNewWF:			.ds 6
			TrkCrntWF:		.ds 6
			TrkEnv:				.ds 6
			TrkPrdUpdate:	.ds 6
			track.mcp:		.ds 1
			TrkCrntPtr:		.ds 2

	.bss
			
			__DMA:							.ds 8
			TileBuffer:					.ds 32
			VColor.lo:					.ds 240
			VColor.hi:					.ds 240
			KeyHold:						.ds 6
			KeyOn:							.ds 6
			TrkDirVol:					.ds 6
			Vibrato:						.ds 6
			VibratoDelay:				.ds 6
			VibratoSpeed:				.ds 6
			VibratoHeight:			.ds 6
			VibratoDelta:				.ds 6
			VibratoDelta.hi:		.ds 6
			TrackPanVolCurrent:	.ds 6
			TrackPanVolUpdate:	.ds 6
			EnvAttackStart:			.ds 6
			EnvAttackDelta:			.ds 6
			EnvDecayDelta:			.ds 6
			TrkPtrStack:				.ds 6
			TrkJsr:							.ds 6
			TrkUpdate:					.ds 6
			TrkFX:							.ds 6
			EnvMode:						.ds 6
			EnvVol:							.ds 6
			BaseVol:						.ds 6
			TrkPanVolLeft:			.ds 6
			TrkPanVolRight:			.ds 6
			TrkPanVolMix:				.ds 6
			TrkMasterPanVolLeft:			.ds 6
			TrkMasterPanVolRight:			.ds 6
			TrkMasterPanVolMix:				.ds 6
			EngineStat:					.ds 1
			tempo:							.ds 1
			TickLen:						.ds 1
			TrkPtrStack.lo:			.ds 6
			TrkPtrStack.hi:			.ds 6
			PrefixCnt:					.ds 1
			
			
			
			MCP:						.ds 1
			
			
			
			
			EndBss:					.ds 1
			
			
			

		

;...............................................................................
;///////////////////////////////////////////////////////////////////////////////.
;///////////////////////////////////////////////////////////////////////////////.
;//Equates  																																  //.
;///////////////////////////////////////////////////////////////////////////////.
;.

		;here
SineWave = $3e00
SineWave2 = $3f00
			
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



;...............................................................................
;///////////////////////////////////////////////////////////////////////////////.
;///////////////////////////////////////////////////////////////////////////////.
;//Self modifying code																											  //.
;///////////////////////////////////////////////////////////////////////////////.
;.


		.code
		.bank 1
		.org $6000 ;.org MCP

;//##################################################################################################
Engine:
			lda EngineStat
		beq .out
			dec tempo
		bne .Player
			lda TickLen
			sta tempo
		jsr Parser
.Player
		jsr Player
.out
	rts




;//##################################################################################################
Player:
			ldx #$05
			stx <track.mcp
.main
			ldx <track.mcp
		jsr .DoUpdate
			ldx <track.mcp
		jsr .DoFX
			ldx <track.mcp
		jsr DoEnv
			dec <track.mcp
		bpl .main
	rts
	
	
	
;//..........................................................
;//..........................................................
.DoUpdate							;updates are cleared upon return of this fuction
			lda TrkUpdate,x
			ldx #$0e
.UpdateLoop
			lsr a
			pha
			phx
		bcc .next_update
			ldy <track.mcp 
		jmp [.UpdateTBL,x]
.next_update
			plx
			pla
			dex 
			dex
		bne .UpdateLoop
			ldx <track.mcp
			stz TrkUpdate,x		
	rts

			
;//..........................................................
.UpdateTBL																												;
	.dw	.cut, .cut, .panvol, .basevol, .keyoff, .waveform, .env, .period	;
;//..........................................................


;//..........................................................
.period
			sxy
			ldy HWChan,x
			sty $800 
			lda TrkPeriod.lo,x
			sta $802
			lda TrkPeriod.hi,x
			sta $803
			lda EnvMode,x
		bne .periodexit
			sxy
		jmp .basevol
.periodexit
		jmp .next_update
		
;//..........................................................
.env	;// this might be useless
			sxy
		jmp .next_update	

;//..........................................................
.waveform
			sxy
			lda <TrkNewWF,x
			cmp <TrkCrntWF,x
		beq .waveformexit	
			sta <TrkCrntWF,x		
			ldy HWChan,x
			sty $800
			ldy #$c0
			sty $804
			stz $804
			ldx #$04
			cly
			cmp #$00
			clc									;see below
		beq .wfload
			ldy #$20
			dec a 
		beq .wfload
			ldy #$40
			dec a
		beq .wfload
			ldy #$60
			dec a
		beq .wfload
		jmp .next_update			;<-error!
.wfload
			lda WaveFormData+0,y
			sta $806
			lda WaveFormData+1,y
			sta $806
			lda WaveFormData+2,y
			sta $806
			lda WaveFormData+3,y
			sta $806
			lda WaveFormData+4,y
			sta $806
			lda WaveFormData+5,y
			sta $806
			lda WaveFormData+6,y
			sta $806
			lda WaveFormData+7,y
			sta $806
			tya
			adc #$08					;carry taken care of
			tay
			dex
		bne .wfload			
.waveformexit
		jmp .next_update	


;//..........................................................
.keyoff
			sxy
			stz KeyOn,x
		jmp .next_update	

;//..........................................................
.basevol
			sxy
			lda BaseVol,x
			sta EnvVol,x
			lsr a
			lsr a
			lsr a
			ora #$80
			sta $804
		jmp .next_update	

;//..........................................................
.panvol
			sxy
			lda TrkPanVolLeft,x
			sec
			sbc TrkDirVol,x
			bcs .floor1
			cla
.floor1
			sta TrkPanVolMix,x
			lda TrkPanVolRight,x
			sec
			sbc TrkDirVol,x
			bcs .floor2
			cla
.floor2
			asl a
			asl a
			asl a
			asl a
			ora TrkPanVolMix,x
			sta TrkPanVolMix,x
			ldy HWChan,x
			sty $800
			sta $805
		jmp .next_update	

;//..........................................................
.cut	
			sxy
			stz EnvMode,x
			ldy HWChan,x
			sty $800
			lda #$c0
			sta $804
			stz $804
		jmp .next_update	
	


;//..........................................................
;//..........................................................
.DoFX
			lda TrkUpdate,x
			ldx #$0e
.FXLoop
			lsr a
			pha
			phx			
		bcc .next_FX
			ldy <track.mcp
		jmp [.FXTBL,x]
.next_FX
			plx
			pla
			dex
			dex
		bne .FXLoop
	rts
	
.FXTBL
	.dw .VibratoFX, .VibratoFX, .VibratoFX, .VibratoFX, .VibratoFX, .VibratoFX, .VibratoFX, .VibratoFX

;//..........................................................
.VibratoFX
			sxy
		jmp .next_FX
	


	
;//..........................................................
DoEnv:
;			lda <TrkEnv,x
;		bne .cont
;	rts
;.cont
			lda KeyOn,x
		beq .cycle_modes
			dec KeyHold,x
		bne .cycle_modes
			stz KeyHold,x
	rts
	
.cycle_modes
			lda EnvMode,x
		beq .disabled
			dec a
		beq .mode1
			dec a
		beq .mode2
			dec a
		beq .mode3
.disabled
	rts

.mode1
			txa
			clc
			adc <TrkEnv,x
			tay
			lda EnvAttackStart,y
			sta EnvVol,x
			inc EnvMode,x
		jmp .UpdateHwVol

.mode2
			txa												;EnvVol = EnvVol + EnvAttackDelta[(TrkEnv*6)+TrkNum]; 
			clc
			adc <TrkEnv,x
			tay
			lda EnvVol,x
			clc
			adc EnvAttackDelta,y
		bcc .nooverflow
			lda #$ff
			inc EnvMode,x
.nooverflow
			sta EnvVol,x
		jmp .UpdateHwVol
		
.mode3
			txa
			clc
			adc <TrkEnv,x
			tay
			lda EnvVol,x
			sec
			sbc EnvDecayDelta,y
		bcs .nounderflow
			lda #$00
			inc EnvMode,x
.nounderflow
			sta EnvVol,x

.UpdateHwVol
			lsr a
			lsr a
			lsr a
			ora #$80
			ldy HWChan,x
			sty $800
			sta $804
	rts		

;//................
HWChan						;
	.db 0,1,2,3,4,5	;
;//................

;//..................
EnvTBL							;
	.db 0,0,6,$c,$12	;
;//..................

;//..........................
EnvSelectTable							;
		.db $00, $06, $0c, $12	;
;//..........................

;//##################################################################################################
Parser:
		
		
			ldx #$05
			stx <TrkNum
Track:
			lda <TrkNum
			asl a
			tax
			lda TrkPtr,x
			sta TrkCrntPtr
			lda TrkPtr+1,x
			sta TrkCrntPtr+1
			stz PrefixCnt
.trackread		
			ldx <TrkNum
			lda [TrkCrntPtr]
		bne .decode		;// rest
.returnincptr
		jsr .IncTrkPtr	
.return
			dec PrefixCnt
			bpl .trackread
			lda <TrkNum
			asl a
			tax
			lda TrkCrntPtr
			sta TrkPtr,x
			lda TrkCrntPtr+1
			sta TrkPtr+1,x			
			dec <TrkNum
		bpl Track
.parserexit		
	rts							;// **** return????
	
.decode
			cmp #$48		
		bcs .next1		;// note
		jmp .long_note
.next1
			cmp #$8f
			;cmp #$8f
		bcs .func		;// short note

.short_note
		sec
		sbc #$48
	jsr .getperiod
		lda EnvMode,x
		cmp #$01
		cla
		adc #$00
		sta EnvMode,x
	jmp .returnincptr	


.long_note
		jsr .getperiod
			;dec a
			;asl a
			;tay
			;lda .period,y
			;sta <TrkPeriod.lo,x
			;lda .period+1,y
			;sta <TrkPeriod.hi,x
			;inc <TrkPrdUpdate,x
		bsr .IncTrkPtr
			lda [TrkCrntPtr]
		beq .finish_track
			sta <TrkAttrib,x
			and #$0f
			asl a
			sax
		jmp [.attrib,x]
.finish_track
		bsr .IncTrkPtr
		jmp .return
			
.func
		jsr .IncTrkPtr
			sec
			sbc #$8f
			asl a
			sax
		jmp [.tbl,x]

.GetByteIncPtr
			lda [TrkCrntPtr]
.IncTrkPtr
			inc TrkCrntPtr
		bne .skip00
			inc TrkCrntPtr+1
.skip00
	rts

.ShiftRight
			lsr a
			dey
		bne .ShiftRight
	rts

.getperiod
			dec a
			asl a
			tay
			lda .period,y
			sta <TrkPeriod.lo,x
			lda .period+1,y
			sta <TrkPeriod.hi,x
			inc <TrkPrdUpdate,x
			lda TrkUpdate,x
			ora #$01
			sta TrkUpdate,x
	rts


;//................................................
.attrib																						;
	.dw .00,.01,.02,.03,.04,.05,.06,.07,.08,.09,.0a	;
;//................................................

		
;//....................................................................................
.tbl																																									;
	.dw .8f																																							;
	.dw .90, .91, .92, .93, .94, .95, .96, .97, .98, .99, .9a, .9b, .9c, .9d, .9e, .9f	;
	.dw .a0, .a1, .a2, .a3, .a4																													;
;//....................................................................................

;//.............................................................................
.period																																				 ;
	 .dw 3421, 3229, 3047, 2877, 2715, 2563, 2419, 2283, 2155, 2034, 1920, 1812	 ;
   .dw 1711, 1615, 1524, 1438, 1358, 1282, 1209, 1142, 1077, 1017, 960 , 906 	 ;
   .dw 855 , 807 , 762 , 719 , 679 , 641 , 605 , 571 , 539 , 508 , 480 , 453	 ;
   .dw 428 , 403 , 381 , 360 , 339 , 320 , 302 , 285 , 269 , 254 , 240 , 227	 ;
   .dw 214 , 202 , 190 , 180 , 170 , 160 , 151 , 143 , 135 , 127 , 120 , 113	 ;
   .dw 107 , 101 , 95  , 90  , 85  , 80  , 76  , 71  , 67  , 64  							 ;
;//.............................................................................



;//..........................................................
.00	
		jmp .finish_track

;//..........................................................
.01		;// Waveform & Env select
			sax
			lda <TrkAttrib,x
			rol a
			rol a
			rol a
			and #$03
			sta <TrkNewWF,x
			lda <TrkAttrib,x
			lsr a
			lsr a
			lsr a
			lsr a
			and #$03
			tay
			lda EnvSelectTable,y
			sta <TrkEnv,x
			stz KeyHold,x
			stz KeyOn,x
			lda TrkUpdate,x
			ora #$06
			sta TrkUpdate,x
			lda #$01
			sta EnvMode,x
		jmp .finish_track

;//..........................................................
.02		;// Direct volume
			sax
			;lda #$ff
			stz EnvMode,x			;//Env off
			lda <TrkAttrib,x
			lsr a
			lsr a
			lsr a
			lsr a
			sta TrkDirVol,x
			stz KeyHold,x
			stz KeyOn,x
			lda TrkUpdate,x
			ora #$30
			sta TrkUpdate,x
		jmp .finish_track

;//..........................................................
.03
			sax
		jmp .finish_track

;//..........................................................
.04		;// Key hold
			sax
			lda <TrkAttrib,x
			lsr a
			lsr a
			lsr a
			lsr a
			sta KeyHold,x
			inc KeyOn,x
		jmp .finish_track

;//..........................................................
.05		;// Vibrato ON
			sax
			lda #$01
			sta Vibrato,x
		jmp .finish_track

;//..........................................................
.06		;// Vibrato OFF
			sax
			stz Vibrato,x
		jmp .finish_track

;//..........................................................
.07
			sax
		jmp .finish_track

;//..........................................................
.08
			sax
		jmp .finish_track

;//..........................................................
.09
			sax
		jmp .finish_track

;//..........................................................
.0a
			sax
		jmp .finish_track
		


		
		
;//********************************************************************************************************
																																																					*
																																																					*
																																																					*
																																																					*
;//********************************************************************************************************



		
		
		

;//..........................................................
.8f		;// Vibrato setup
			sax
		jsr .GetByteIncPtr
			sta VibratoDelay,x
		jsr .GetByteIncPtr
			sta VibratoHeight,x
		jsr .GetByteIncPtr
			sta VibratoDelta,x
			stz VibratoDelta.hi,x
			rol a
			bcc .vbpos
			dec VibratoDelta.hi,x
.vbpos
		jsr .GetByteIncPtr
			sta VibratoSpeed,x
		jmp .return

;//..........................................................
.90		;//Env 0 define
			sax
			txa
			tay
			bra .env_define
.91		;//Env 1 define
			sax
			txa
			adc #$06
			tay
			bra .env_define
.92		;//Env 2 define
			sax
			txa
			adc #$0c
			tay

.env_define
		jsr .GetByteIncPtr
			sta EnvAttackStart,y
		jsr .GetByteIncPtr
			sta EnvAttackDelta,y
		jsr .GetByteIncPtr
			sta EnvDecayDelta,y
		jmp .return

;//..........................................................
.93		;// Key hold (redundant)
			sax	
		jsr .GetByteIncPtr
			sta KeyHold,x
			stz KeyOn,x
			inc KeyOn,x
		jmp .return

;//..........................................................
.94		;// Key release
			sax	
		jsr .GetByteIncPtr
			stz KeyHold,x
			stz KeyOn,x
		jmp .return

;//..........................................................
.95		;// Note Cut
			sax
			stz TrackPanVolCurrent,x
			lda #$01
			sta TrackPanVolUpdate,x
			lda TrkUpdate,x
			ora #$40
			sta TrkUpdate,x
		jmp .return
		
;//..........................................................
.96		;// Call
			sax

			lda #$01
			sta TrkJsr,x

			lda <TrkCrntPtr
			sec
			sbc #$01
			sta TrkPtrStack.lo,x
			lda <TrkCrntPtr+1
			sbc #$00
			sta TrkPtrStack.hi,x
			jsr .change_ptr 
		jmp .trackread					

.change_ptr
		jsr .GetByteIncPtr
			pha
		jsr .GetByteIncPtr
			sta <TrkCrntPtr+1
			pla
			sta <TrkCrntPtr
	rts			

;//..........................................................
.97		;// RTS
			sax
			lda TrkPtrStack.lo,x
			sta <TrkCrntPtr
			lda TrkPtrStack.hi,x
			sta <TrkCrntPtr+1
			stz TrkJsr,x
		jmp .trackread			

;//..........................................................
.98		;// JMP
			sax
		jsr .change_ptr
		jmp .trackread			

;//..........................................................
.99		;// Base volume
			sax
		jsr .GetByteIncPtr
			sta BaseVol,x
			;lda TrkUpdate,x
			;ora #$20
			;sta TrkUpdate,x			
		jmp .return
		
;//..........................................................
.9a		;// Pan volume
			sax
		jsr .GetByteIncPtr
			eor #$ff
			sta TrkPanVolMix,x
			and #$0f
			sta TrkPanVolRight,x
			lda TrkPanVolMix,x
			lsr a
			lsr a
			lsr a
			lsr a
			sta TrkPanVolLeft,x
			lda TrkUpdate,x
			ora #$20
			sta TrkUpdate,x			
		jmp .return
		
;//..........................................................
.9b		;// WF select
			sax
		jsr .GetByteIncPtr
			and #$03
			sta <TrkNewWF,x
			lda TrkUpdate,x
			ora #$04
			sta TrkUpdate,x			
		jmp .return
		
;//..........................................................
.9c		;// Tick set
			sax
		jsr .GetByteIncPtr
			and #$1f
			sta TickLen
			sta tempo
		jmp .return
		
;//..........................................................
.9d		;// Prefix
			sax
		jsr .GetByteIncPtr
			and #$7f
			sta PrefixCnt
		jmp .return
		
;//..........................................................
.9e		;// Master Pan
			sax
		jsr .GetByteIncPtr
			sta TrkMasterPanVolMix,x
			stz $800
			sta $801
			
		jmp .return
		
;//..........................................................
.9f		;// Volume slide up
			sax
			lda TrkDirVol,x
			inc a
			cmp #$10
			bcs .9f_skip
			lda #$0f
.9f_skip
			sta TrkDirVol,x
			lda TrkUpdate,x
			ora #$20
			sta TrkUpdate,x
			
		jmp .return

;//..........................................................
.a0		;// Volume slide down
			sax
			lda TrkDirVol,x
			dec a
			bpl .a0_skip
			cla
.a0_skip
			sta TrkDirVol,x
			lda TrkUpdate,x
			ora #$20
			sta TrkUpdate,x
		jmp .return

;//..........................................................
.a1		;// Direct volume set
			sax
		jsr .GetByteIncPtr
			and #$0f
			sta TrkDirVol,x
			lda TrkUpdate,x
			ora #$10
			sta TrkUpdate,x
		jmp .return

;//..........................................................
.a2		;// Direct volume mode on
			sax
			stz EnvMode,x			;//Env off
			stz KeyHold,x
			stz KeyOn,x
			;lda TrkUpdate,x
			;ora #$30
			;sta TrkUpdate,x
		jmp .return

;//..........................................................
.a3		;// Envelope mode on
			sax
			lda #$04
			sta EnvMode,x			;//Env standby
		jmp .return

;//..........................................................
.a4		;// Envelope select
			sax
		jsr .GetByteIncPtr
			and #$03
			tay			
			lda EnvSelectTable,y
			sta TrkEnv,x
		jmp .return




;.......................................
WaveFormData:													 ;
																			 ;
	.db	$00,$00,$00,$00,$00,$00,$00,$00	 ;	Square 50%
	.db	$00,$00,$00,$00,$00,$00,$00,$00	 ;
	.db	$1f,$1f,$1f,$1f,$1f,$1f,$1f,$1f	 ;
	.db	$1f,$1f,$1f,$1f,$1f,$1f,$1f,$1f	 ;
																			 ;
	.db	$00,$00,$00,$00,$00,$00,$00,$00	 ;	Square 37%
	.db	$00,$00,$00,$00,$00,$00,$00,$00	 ;
	.db	$00,$00,$00,$00,$10,$1f,$1f,$1f	 ;
	.db	$1f,$1f,$1f,$1f,$1f,$1f,$1f,$1f	 ;
																			 ;
	.db $00,$01,$02,$03,$04,$05,$06,$07  ;
	.db $08,$09,$0a,$0b,$0c,$0d,$0e,$0f  ;	Saw (ramp up)
	.db $10,$11,$12,$13,$14,$15,$16,$17  ;
	.db $18,$19,$1a,$1b,$1c,$1d,$1f,$1e  ;
																			 ;
	.db $06,$06,$07,$08,$09,$0b,$0d,$13  ;	Sine
	.db $11,$12,$13,$14,$14,$15,$16,$16  ;
	.db $17,$17,$18,$18,$17,$16,$15,$13  ;
	.db $11,$0f,$0d,$0b,$09,$08,$07,$06  ;
;.......................................
	
	
	
;//##################################################################################################
;

	.bank $02
	.org $4000

TestSong:

SongTrack0:
			.db PreFix.,$08, TickSet.,$07, BaseVol.,$ff, PanVol.,$14, WFset.,$02, MasterPan.,$ff
			.db EnvDef1., $00, $a0,$08, EnvSelect., $01, EnvMode.

			;.db A.3, $11, Rest.,Rest.,Rest.,Rest.,Rest.,Rest.,Rest.,Rest.,Rest.,Rest.
			;.db F.2, $12
			;.db DirVolMode.
			
.lp01			
			
			.db Call., low(loop1),high(loop1)
			.db Jump.,low(.lp01),high(.lp01)
loop1:			
			.db _F.4
			.db _Gx4
			.db _C.5
			.db _F.5
			.db _Gx5
			.db _F.5
			.db _C.5
			.db _Gx4
			.db _F.4
			.db _Gx4
			.db _C.5
			.db _F.5
			.db _Gx5
			.db _F.5
			.db _C.5
			.db _Gx4
			.db _F.4
			.db _C.5
			.db _Dx5
			.db _Gx5
			.db _C.6
			.db _Gx5
			.db _Dx5
			.db _C.5
			.db _Gx4
			.db _C.5
			.db _Dx5
			.db _Gx5
			.db _C.6
			.db _Gx5
			.db _Dx5
			.db _C.5
			.db Return.

SongTrack1:
			.db PreFix.,$07, BaseVol.,$ff, PanVol.,$45, WFset.,$02, MasterPan.,$ff
			.db EnvDef1., $f8, $08,$1f, EnvSelect., $01, DirVolMode.
			.db Rest., Rest.
.lp01			
			.db Call., low(loop1),high(loop1)
			.db Jump.,low(.lp01),high(.lp01)




SongTrack2:
			.db PreFix.,$07, BaseVol.,$ff, PanVol.,$78, WFset.,$02, MasterPan.,$ff
			.db EnvDef1., $08, $80,$1f, EnvSelect., $01, DirVolMode.

			.db Rest., Rest., Rest., Rest.
.lp01			
			.db Call., low(loop1),high(loop1)
			.db Jump.,low(.lp01),high(.lp01)



SongTrack3:

			.db $00, $98,low(SongTrack3),high(SongTrack3)


SongTrack4:

			.db $00, $98,low(SongTrack4),high(SongTrack4)

SongTrack5:

			.db $00, $98,low(SongTrack5),high(SongTrack5)


	
	
	
	
;notes
C.1=$01     
Cx1=$02
D.1=$03
Dx1=$04
E.1=$05
F.1=$06
Fx1=$07    
G.1=$08
G1x=$09    
A.1=$0a    
Ax1=$0b
B.1=$0c
C.2=$0d    
Cx2=$0e
D.2=$0f
Dx2=$10
E.2=$11
F.2=$12
Fx2=$13    
G.2=$14
Gx2=$15    
A.2=$16    
Ax2=$17
B.2=$18
C.3=$19   
Cx3=$1a
D.3=$1b
Dx3=$1c
E.3=$1d
F.3=$1e
Fx3=$1f    
G.3=$20
Gx3=$21    
A.3=$22    
Ax3=$23
B.3=$24
C.4=$25     
Cx4=$26
D.4=$27
Dx4=$28
E.4=$29
F.4=$2a
Fx4=$2b    
G.4=$2c
Gx4=$2d    
A.4=$2e    
Ax4=$2f
B.4=$30
C.5=$31   
Cx5=$32
D.5=$33
Dx5=$34
E.5=$35
F.5=$36
Fx5=$37    
G.5=$38
Gx5=$39    
A.5=$3a    
Ax5=$3b
B.5=$3c
C.6=$3d     
Cx6=$3e
D.6=$3f
Dx6=$40
E.6=$41
F.6=$42
Fx6=$43    
G.6=$44
Gx6=$45    
A.6=$46    
Ax6=$47

_C.1=$01+$47     
_Cx1=$02+$47
_D.1=$03+$47
_Dx1=$04+$47
_E.1=$05+$47
_F.1=$06+$47
_Fx1=$07+$47
_G.1=$08+$47
_Gx1=$09+$47
_A.1=$0a+$47    
_Ax1=$0b+$47
_B.1=$0c+$47
_C.2=$0d+$47    
_Cx2=$0e+$47
_D.2=$0f+$47
_Dx2=$10+$47
_E.2=$11+$47
_F.2=$12+$47
_Fx2=$13+$47    
_G.2=$14+$47
_Gx2=$15+$47    
_A.2=$16+$47    
_Ax2=$17+$47
_B.2=$18+$47
_C.3=$19+$47   
_Cx3=$1a+$47
_D.3=$1b+$47
_Dx3=$1c+$47
_E.3=$1d+$47
_F.3=$1e+$47
_Fx3=$1f+$47    
_G.3=$20+$47
_Gx3=$21+$47    
_A.3=$22+$47    
_Ax3=$23+$47
_B.3=$24+$47
_C.4=$25+$47     
_Cx4=$26+$47
_D.4=$27+$47
_Dx4=$28+$47
_E.4=$29+$47
_F.4=$2a+$47
_Fx4=$2b+$47    
_G.4=$2c+$47
_Gx4=$2d+$47    
_A.4=$2e+$47    
_Ax4=$2f+$47
_B.4=$30+$47
_C.5=$31+$47   
_Cx5=$32+$47
_D.5=$33+$47
_Dx5=$34+$47
_E.5=$35+$47
_F.5=$36+$47
_Fx5=$37+$47    
_G.5=$38+$47
_Gx5=$39+$47    
_A.5=$3a+$47    
_Ax5=$3b+$47
_B.5=$3c+$47
_C.6=$3d+$47     
_Cx6=$3e+$47
_D.6=$3f+$47
_Dx6=$40+$47
_E.6=$41+$47
_F.6=$42+$47
_Fx6=$43+$47    
_G.6=$44+$47
_Gx6=$45+$47    
_A.6=$46+$47    
_Ax6=$47+$47

Rest. = $00
VbrDef. = $8f
EnvDef0. = $90
EnvDef1. = $91
EnvDef2. = $92
KeyHold. = $93
KeyRls.	= $94
NoteCut. = $95
Call. = $96
Return. = $97
Jump. = $98
BaseVol. = $99
PanVol. = $9a
WFset. = $9b
TickSet. = $9c
PreFix. = $9d
MasterPan. = $9e
DirVolSet. = $a1
DirVolMode. = $a2
EnvMode. = $a3
EnvSelect. = $a4

