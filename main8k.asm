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
		incbin "hes.hdr"
    include "_startup.asm"
    .code
  



;*******************************************************************************
;///////////////////////////////////////////////////////////////////////////////*
;//MAIN                                                                       //*
;///////////////////////////////////////////////////////////////////////////////*
;.
	.db "  main__",0
main:                             

      jsr init_vdc
      jsr init_wsg

      VCE_REG 	LO_RES|H_FILTER
      VDC_REG 	DCR , AUTO_SATB_ON          
      VDC_REG 	CR ,  $0000
      VDC_REG 	RCR , $0000
      IRQ_CNTR 	IRQ2_OFF|VIRQ_ON|TIRQ_ON
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
			inx
			cpx #$80
		bcc .clear_cram
			

			
			
			lda #$23
			sta <R3
			
      VDC_REG 	CR , BG_ON|SPR_ON|VINT_ON|HINT_ON

			
			lda #$ff
			sta VColor.lo+$40

			stz KeyHold
			tii KeyHold,KeyHold+1,$500
			

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

			stz TrkRestLoop+0
			stz TrkRestLoop+1
			stz TrkRestLoop+2
			stz TrkRestLoop+3
			stz TrkRestLoop+4
			stz TrkRestLoop+5

			stz PortaFX+0
			stz PortaFX+1
			stz PortaFX+2
			stz PortaFX+3
			stz PortaFX+4
			stz PortaFX+5
			
			stz <DDAon

			lda #low(SongDepack)
			sta LZDEST
			lda #high(SongDepack)
			sta LZDEST+1
			ldy #low(Unreal_Superhero_3)
			ldx #high(Unreal_Superhero_3)
		jsr pu_decmp


			cli

			
			
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.testmusicengine			
		jsr WaitVblank
		jsr Engine
		jmp .testmusicengine	
			

		jmp .testmusicengine


;...............................................................................
;End MAIN



;...............................................................................
;///////////////////////////////////////////////////////////////////////////////.
;///////////////////////////////////////////////////////////////////////////////.
;//Main library																															  //.
;///////////////////////////////////////////////////////////////////////////////.
;.

	.db "  pudecmp__",0
	.include "pce_decmp.asm"
	.db "  init_hw__",0
	.include "init_hw.asm"
	.db "  unreal__",0
Unreal_Superhero_3:
	.incbin "unreal.lzss"
	
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
WaitVblank:
			stz <__vblank
			inc <__vblank
.loop
			tst #$01, <__vblank
			bne .loop
	rts
.WaitVblankEnd



;		.code
;		.bank 1
;		.org $6000 ;.org MCP

	.db "  mcp__",0
;//##################################################################################################
Engine:
			stz $c00
			stz $c01
			stz $1403
			lda #$01
			sta $c01
			stz $c00
			stz $1403
			lda EngineStat
		beq .out
			dec tempo
		bne .Player
			lda TickLen
			sta tempo
		jsr Parser
.Player
			sei
		jsr Player
			cli
.out
	rts


;//##################################################################################################
DDATimer:
			stz $1403
			tst #$01, <DDAon
		bne .DDAon
	rti
.DDAon
			pha
			tma #$05
			pha
			lda <DDAbank
			tam #$05
			lda #$05
			sta $800
			lda #$df
			sta $804
			lda [DDAptr]
		bmi .end
			sta $806
			inc <DDAptr
		beq .msb
		bra .out
.msb
			lda <DDAptr+1
			inc a
			cmp #$E0
			bcs .bank
.fin
			sta <DDAptr+1
.out			
			pla
			tam #$05
			pla
	rti
.bank	
			inc <DDAbank
		bra .fin

.end
			stz <DDAon
		bra .fin


;//##################################################################################################
Player:
			ldx #$05
			stx <track.mcp
.main
			ldx <track.mcp
		Jsr DoVolSlide
			ldx <track.mcp
		Jsr DoArp
			ldx <track.mcp
		jsr .DoUpdate
;			ldx <track.mcp
;		jsr .DoFX
			ldx <track.mcp
		jsr DoEnv
			ldx <track.mcp
		jsr DoPorta
			ldx <track.mcp
		jsr VibratoFX
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
		jsr .DoUpdateSub
.next_update
			plx
			pla
			dex 
			dex
		bne .UpdateLoop
			ldx <track.mcp
			stz TrkUpdate,x		
	rts

.DoUpdateSub
		jmp [.UpdateTBL,x]
			
;//..........................................................
.UpdateTBL																												;
	.dw	.cut, .cut, .panvol, .basevol, .keyoff, .waveform, .env, .period	;
;//..........................................................


;//..........................................................
.period
			;sxy
			;ldy HWChan,x
			sty $800
			lda TrkNoteTemp,y 
			sta TrkCurrentNote,y
			lda TrkPeriodLoad.lo,y
			sta TrkPeriod.lo,y
			sta $802
			lda TrkPeriodLoad.hi,y
			sta TrkPeriod.hi,y
			sta $803
			lda EnvMode,y
		bne .periodexit
			;sxy
		bra .basevol
.periodexit
	rts		

;//..........................................................
.env	;// this might be useless
	rts		

;//..........................................................
.waveform
			sxy
			lda <TrkNewWF,x
			cmp <TrkCrntWF,x
		beq .waveformexit	
			sta <TrkCrntWF,x		
			;ldy HWChan,x
			stx $800
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
	rts		
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
	rts		


;//..........................................................
.keyoff
			sxy
			stz KeyOn,x
	rts		

;//..........................................................
.basevol
			;sxy
			lda BaseVol,y
			sta EnvVol,y
			lsr a
			lsr a
			lsr a
			ora #$80
			sta $804
	rts		

;//..........................................................
.panvol
			;sxy
			lda TrkPanVolLeft,y
			sec
			sbc TrkDirVol,y
			bcs .floor1
			cla
.floor1
			lsr a
			lsr a
			lsr a
			lsr a
			sta TrkPanVolMix,y
			lda TrkPanVolRight,y
			sec
			sbc TrkDirVol,y
			bcs .floor2
			cla
.floor2
			and #$f0
			ora TrkPanVolMix,y
			sta TrkPanVolMix,y
			;ldy HWChan,x
			sty $800
			sta $805
	rts		

;//..........................................................
.cut	
			sxy
			stz EnvMode,x
			;ldy HWChan,x
			lda EnvVol,x
			lsr a
			lsr a
			lsr a
			ora #$c0
			stx $800
			sta $804
			lda #$0f
			sta $806
	rts		
	
	
	
	
	


;//..........................................................
;//..........................................................
;.DoFX
;			lda TrkUpdate,x
;			ldx #$0e
;.FXLoop
;			lsr a
;			pha
;			phx			
;		bcc .next_FX
;			ldy <track.mcp
;		jmp [.FXTBL,x]
;.next_FX
;			plx
;			pla
;			dex
;			dex
;		bne .FXLoop
;	rts
	
;.FXTBL
;	.dw .VibratoFX, .VibratoFX, .VibratoFX, .VibratoFX, .VibratoFX, .VibratoFX, .VibratoFX, .VibratoFX

;//..........................................................
VibratoFX:
			

			stx $800
			lda VbrPtr.lo,x
			sta VbrPtr
			lda VbrPtr.hi,x
			sta VbrPtr+1

			lda VbrFX,x
			bit #$80
		bne .out
			tay
			lda tempo
			cmp #$01
		bne .DoVbr
			lda VbrFX,x
			ora #$80
			sta VbrFX,x
.DoVbr
			tya		
			bit #$40
		beq .out
			and #$03
			;cmp #$00
		beq .VbrFX01
			dec a
		beq .VbrFX02
			dec a
		beq .VbrFX03
			dec a
		beq .VbrFX04
			
.out
		rts


.VbrFX01									; 1st quarter sine
		bsr .1
		bra .inc
			
.VbrFX02									; 2nd quarter sine
		bsr .1
			tya
			sec
			sbc VbrSpeed,x
		bcs .VbrUpdate
			lda VbrIndex,x
			inc VbrFX,x
		bra .VbrUpdate


.VbrFX03									; 3nd quarter sine
		bsr .2
		bra .inc

.VbrFX04									; 4th quarter sine
		bsr .2
			tya
			sec
			sbc VbrSpeed,x
		bcs .VbrUpdate
			lda VbrIndex,x
			tay
			lda VbrFX,x
			and #$fc
			sta VbrFX,x
.VbrUpdate2
			tya
.VbrUpdate
			sta VbrIndex,x
			lda TrkPeriod.lo,x
			clc
			adc TrkVbrDelta.lo,x
			sta $802
			lda TrkPeriod.hi,x
			adc TrkVbrDelta.hi,x
			sta $803
		rts

.delay
			dec VbrCntr,x
		bne .cont
			lda VbrSpeed,x
			sta VbrCntr,x
			clc
	rts
.cont			
			sec
	rts
	
.1
			ldy VbrIndex,x
			lda [VbrPtr],y
			sta TrkVbrDelta.lo,x
			stz TrkVbrDelta.hi,x
	rts

.2
			ldy VbrIndex,x
			cla
			sec
			sbc [VbrPtr],y
			sta TrkVbrDelta.lo,x
			cla
			sbc #$00
			sta TrkVbrDelta.hi,x
	rts	
	
.inc
			tya
			clc
			adc VbrSpeed,x
			cmp #$10
		bcc .VbrUpdate
			lda VbrIndex,x
			inc VbrFX,x
		bra .VbrUpdate

	
;//..........................................................
;.DoMasterFade
;			sxy
;	rts

;//..........................................................
DoPorta:
;
;
;
;
			lda PortaFX,x
			bit #$80
		bne .out
			bit #$40
		beq .process
.step										;<- note/line entry stepping
		jsr .process2
			lda tempo
			cmp #$01
		bne .notexpired
		jsr PausePortaSub
.notexpired
	rts

.process
			bit #$10
		bne .out						;<- pause auto mode
.process2		
			and #$0f
		beq .out
			dec a
		beq .porta
			dec a
		beq .portanoteup
			dec a
		beq .portanotedown
			;else ERROR
.out
	rts

.porta			
			stx $800
			lda TrkPeriod.lo,x
			clc
			adc PortaDelta.lo,x
			sta TrkPeriod.lo,x
			sta $802
			lda TrkPeriod.hi,x
			adc PortaDelta.hi,x
			sta TrkPeriod.hi,x
			sta $803
	rts

.portanoteup
		bsr .add1
		beq .checklsbup
		bcc .overflowup
.writeup
		bra .write
	rts
.checklsbup
			say
			cmp PortaNote.lo,x
			say
		bcc .overflowup
		bra .writeup
.overflowup
		bra .overflow
			
.portanotedown
		bsr .add1
		beq .checklsbdown
		bcs .overflowdown
.writedown
.write
			stx $800
			sta TrkPeriod.hi,x
			sta $803
			tya
			pha
			pla
			sta $802
			sta TrkPeriod.lo,x
	rts
.checklsbdown
			say
			cmp PortaNote.lo,x
			say
		bcs .overflowdown
		bra .writedown
.overflowdown
.overflow
			lda PortaFX,x
			and #$f0
			sta PortaFX,x
			ldy PortaNote.lo,x
			lda PortaNote.hi,x
		bra .writedown

.add1
			lda TrkPeriod.lo,x
			clc
			adc PortaDelta.lo,x
			tay
			lda TrkPeriod.hi,x
			adc PortaDelta.hi,x
			cmp PortaNote.hi,x
	rts
	
;//..........................................................
DoEnv:
;
;
;
;
			lda KeyOn,x
		beq .cycle_modes
			bit #$80
		bne .cycle_modes
			dec KeyHold,x
		bpl .cycle_modes
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
			dec a
		beq .mode4
.disabled
	rts

.mode1
		bsr .add1
			lda EnvAttackStart,y
			sta EnvVol,x
			inc EnvMode,x
		bra .UpdateHwVol

.mode2
		bsr .add1											;EnvVol = EnvVol + EnvAttackDelta[(TrkEnv*6)+TrkNum]; 
			lda EnvVol,x
			clc
			adc EnvAttackDelta,y
		bcc .nooverflow
			lda #$ff
			inc EnvMode,x
.nooverflow
			sta EnvVol,x
		bra .UpdateHwVol
		
.mode3
		bsr .add1
			lda EnvLevel,y
			sta EnvVol,x
			inc EnvMode,x
		bra .UpdateHwVol
		
		
.mode4
		bsr .add1
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
			;ldy HWChan,x
			stx $800
			sta $804
	rts		

.add1
			txa
			clc
			adc <TrkEnv,x
			tay
	rts



;//..........................................................
DoArp:
;
;
;
;

			lda ArpMode,x
		beq .out		;//disabled
			bit #$80
		bne .out		;//paused
			bit #$40	
		bne .step		;//first call
			and #$0f
			dec a
		beq	.step		;//note trigger
			dec a
		beq	.step		;//auto
			dec a
		beq .out 		;// to do
.out
	rts
	
.step
			lda ArpCntr,x
			inc ArpCntr,x
			cmp #$01
		bcs .step.next
			lda Arp1,x
		bra .step.update
.step.next
			cmp #$02
		bcs .step.next2	
			lda Arp2,x
			clc
			adc Arp1,x
		bra .step.update
.step.next2		
			cmp #$03
		bcs .step.next3	
			lda Arp3,x
			clc
			adc Arp1,x
		bra .step.update
.step.next3
			stz ArpCntr,x
		bra .step

.step.update
		jsr GetPeriod2
			lda tempo
			cmp #$01
		bne .step.cont
			lda ArpMode,x
			ora #$80
			sta ArpMode,x
			stz ArpCntr,x
.step.cont
	rts

;//..........................................................
DoVolSlide:
;
;
;
;

			lda VolSlideMode,x
		beq .out
			bit #$80
		bne .out
			bit #$10
		bne .delay
			bit #$40
		bne .Up
			bit #$20
		bne .Down
		;else error
.out
	rts
	
.delay
			and #$ef
			sta VolSlideMode,x
	rts	

.Up
			lda TrkDirVol,x
			sec
			sbc VolSlideArg,x
		bcs .skip
			cla
.skip
			sta TrkDirVol,x
		bra .update

.Down
			lda TrkDirVol,x
			clc
			adc VolSlideArg,x
		bcc .skip1
			lda #$ff
.skip1
			sta TrkDirVol,x
		
.update
		jsr PanUpdateSub
			lda tempo
			cmp #$01
		bne .slide.cont
			lda VolSlideMode,x
			ora #$80
			sta VolSlideMode,x
.slide.cont
			
	rts
	

;//---------------
; Some public functions	

PanUpdateSub:
			lda TrkUpdate,x
			ora #$20
			sta TrkUpdate,x
	rts

ResetKeySub:
			lda KeyOn,x
			and #$7f
			sta KeyOn,x
			lda KeyHoldInit,x
			sta KeyHold,x
	rts

PausePortaSub:
			lda PortaFX,x
			ora #$80
			sta PortaFX,x
	rts

ArpStepSub:
			lda ArpMode,x
			and #$7f
			ora #$40
			sta ArpMode,x
			stz ArpCntr,x
			lda TrkNoteTemp,x
			sta Arp1,x
	rts

;//................
HWChan						;
;	.db 0,1,2,3,4,5	;
;//................

;//..................
EnvTBL							;
	.db 0,0,6,$c,$12	;
;//..................

;//..........................
EnvSelectTable							;
		.db $00, $06, $0c, $12	;
;//..........................

;//.............................................................................
period																																				 ;
	 .dw 3421, 3229, 3047, 2877, 2715, 2563, 2419, 2283, 2155, 2034, 1920, 1812	 ;
   .dw 1711, 1615, 1524, 1438, 1358, 1282, 1209, 1142, 1077, 1017, 960 , 906 	 ;
   .dw 855 , 807 , 762 , 719 , 679 , 641 , 605 , 571 , 539 , 508 , 480 , 453	 ;
   .dw 428 , 403 , 381 , 360 , 339 , 320 , 302 , 285 , 269 , 254 , 240 , 227	 ;
   .dw 214 , 202 , 190 , 180 , 170 , 160 , 151 , 143 , 135 , 127 , 120 , 113	 ;
   .dw 107 , 101 , 95  , 90  , 85  , 80  , 76  , 71  , 67  , 64  							 ;
;//.............................................................................

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
			ldx <TrkNum
			tst #$80, TrkJsr,x
		beq .trackread
			dec TrkCallLen,x
		bne .trackread
			sax
		jmp .97
.trackread		
			ldx <TrkNum
			lda TrkRestLoop,x
		beq .noRestLoop
			dec TrkRestLoop,x
		bra	.return
.noRestLoop
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
	
.jmpfunc
		jmp .func	
	
.decode
			cmp #$48		
		bcs .next1		;// note
		jmp .long_note
.next1
			cmp #$8f
		bcs .jmpfunc		;// short note

.short_note
			stx $800
			cpx #$05
			bne .skipshortdda
			stz <DDAon 
.skipshortdda
			sec
			sbc #$47
			sta TrkNoteTemp,x
			dec TrkNoteTemp,x
		jsr .getperiod
			lda EnvMode,x
			cmp #$01
			cla
			adc #$00
			sta EnvMode,x
			lda PortaFX,x
			bit #$20
		beq .short_note.skip
			lda PortaFX,x
			and #$3f
			sta PortaFX,x
		jsr .0e.shortnote		
		bra .ArpCheck
.short_note.skip
		jsr PausePortaSub
.ArpCheck
			lda ArpMode,x
			bit #$10
			beq .skipArp0
			and #$7f
			ora #$40
			sta ArpMode,x
			lda TrkNoteTemp,x
			sta Arp1,x
			stz ArpCntr,x
.skipArp0
		jsr ResetKeySub
			stz TrkDirVol,x
		jsr PanUpdateSub
;			lda #$d1
;			sta $804
;			lda #$10
;			sta $806
		jmp .returnincptr	
		

.long_note
			cpx #$05
			bne .skiplongdda
			stz <DDAon 
.skiplongdda
			stz TrueNote,x
			sta TrkNoteTemp,x
			dec TrkNoteTemp,x
		jsr .getperiod
		bsr .IncTrkPtr
		jsr PausePortaSub
			lda EnvMode,x
			cmp #$01
			cla
			adc #$00
			sta EnvMode,x
			lda [TrkCrntPtr]
		beq .finish_track
			sta <TrkAttrib,x
			and #$0f
			asl a
			sax
		jsr .DoAttrib

.finish_track
		bsr .IncTrkPtr
			ldx <TrkNum
			tst #$ff, TrueNote,x
			beq .fakenote
			lda TrkNoteTemp,x
			sta Arp1,x
.fakenote
			lda ArpMode,x
			bit #$10
			beq .skipArp1
			and #$7f
			ora #$40
			sta ArpMode,x
			stz ArpCntr,x
.skipArp1
		jsr ResetKeySub
			tst #$20, TrkUpdate,x
		bne .skipvolreset
			stz TrkDirVol,x
		jsr PanUpdateSub
.skipvolreset
;			lda #$d1
;			sta $804
;			lda #$10
;			sta $806
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

;.ShiftRight
;			lsr a
;			dey
;		bne .ShiftRight
;	rts

.getperiod
			dec a
.getperiod2
			asl a
			tay
			lda period,y
			sta TrkPeriodLoad.lo,x
			lda period+1,y
			sta TrkPeriodLoad.hi,x
			lda TrkUpdate,x
			ora #$01
			sta TrkUpdate,x
	rts

GetPeriod2 = .getperiod2

.DoAttrib
		jmp [.attrib,x]


;//....................................................................
.attrib																																;
	.dw .00,.01,.02,.03,.04,.05,.06,.07,.08,.09,.0a,.0b,.0c,.0d,.0e,.0f	;
;//....................................................................

		
;//....................................................................................
.tbl																																									;
	.dw .8f																																							;
	.dw .90, .91, .92, .93, .94, .95, .96, .97, .98, .99, .9a, .9b, .9c, .9d, .9e, .9f	;
	.dw .a0, .a1, .a2, .a3, .a4, .a5, .a6, .a7, .a8, .a9, .aa, .ab, .ac, .ad, .ae, .af	;
	.dw .b0, .b1, .b2, .b3, .b4, .b5, .b6, .b7, .b8, .b9, .ba, .bb, .bc, .bd, .be				;
;//....................................................................................




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
		bsr .trackhinybble
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
	rts

;//..........................................................
.02		;// Direct volume mode
			sax
			stz EnvMode,x			;//Env off
			lda <TrkAttrib,x
			and #$f0
			sta TrkDirVol,x
			stz KeyHold,x
			stz KeyOn,x
			lda TrkUpdate,x
			ora #$30
			sta TrkUpdate,x
	rts

;//..........................................................
.03		;// Direct volume set
			sax
			lda <TrkAttrib,x
			and #$f0
			sta TrkDirVol,x
		jsr PanUpdateSub

	rts

;//..........................................................
.04		;// Key hold
			sax
		bsr .trackhinybble
			sta KeyHold,x
			inc KeyOn,x
	rts

.trackhinybble
			lda <TrkAttrib,x
			lsr a
			lsr a
			lsr a
			lsr a
	rts

;//..........................................................
.05		;// Vibrato ON
			sax
			lda #$01
			;sta Vibrato,x
	rts

;//..........................................................
.06		;// Vibrato OFF
			sax
			;stz Vibrato,x
	rts

;//..........................................................
.07		;// VolSlide up Arg
			sax
		bsr .volslidecmp
			ora #$50
		bra .volslidefinish

;//..........................................................
.08		;// VolSlide Down Arg
			sax
		bsr .volslidecmp
			ora #$30
.volslidefinish
			sta VolSlideMode,x
			stz TrkDirVol,x
	rts

.volslidecmp
			lda <TrkAttrib,x
			lsr a
			lsr a
			lsr a
			and #$78
			sta VolSlideArg,x
			lda VolSlideMode,x
			and #$1F
	rts
;//..........................................................
.09
			sax
	rts

;//..........................................................
.0a
			sax
	rts
		
;//..........................................................
.0b		;// step Arpeggio 
			sax
			jmp ArpStepSub

;//..........................................................
.0c		;// Portamento Step
			sax
			lda PortaFX,x
			and #$7f
			ora #$40
		bra .portafinish
;//..........................................................
.0d		;// Portamento Off
			sax
			lda PortaFX,x
			and #$80
.portafinish
			sta PortaFX,x
	rts
;//..........................................................
.0e		;// Portamento On

;		jsr .0e.doporta
;	rts
.0e.doporta
			sax
.0e.shortnote
			lda <TrkAttrib,x
			and #$f0
		bne .0e.on
.0e.off
		jsr PausePortaSub
	rts
.0e.on
			lsr a
			lsr a
			lsr a
			lsr a
			dec a
			beq .0e.up
			dec a
			beq .0e.down
			dec a
			beq .0e.note
			lda PortaFX,x			;<- error
			and #$80
			sta PortaFX,x
		rts
			
.0e.up
			inc a
			ora PortaFX,x
			and #$7f
			sta PortaFX,x
			lda TrkPortaArg,x
			eor #$ff
			clc
			adc #$01
			sta PortaDelta.lo,x
			lda #$ff
			adc #$00
			sta PortaDelta.hi,x
		rts
.0e.down
			inc a
			inc a
			ora PortaFX,x
			and #$7f
			sta PortaFX,x
			lda TrkPortaArg,x
			sta PortaDelta.lo,x
			stz PortaDelta.hi,x
		rts
.0e.note
			inc a
			inc a
			inc a
			ora PortaFX,x
			and #$7f
			sta PortaFX,x
			lda TrkUpdate,x
			and #$fe
			sta TrkUpdate,x
			lda TrkNoteTemp,x
			asl a
			tay
			lda period,y
			sta PortaNote.lo,x
			lda period+1,y			
			sta PortaNote.hi,x
			tya
			lsr a
			sta TrkPortaNote,x
			cmp TrkCurrentNote,x
			sta TrkCurrentNote,x
			beq .0e.note.out
			bcs .0e.note.up
.0e.note.down
			lda TrkPortaArg,x
			sta PortaDelta.lo,x
			stz PortaDelta.hi,x
		rts

.0e.note.up			
			dec PortaFX,x
			lda TrkPortaArg,x
			eor #$ff
			clc
			adc #$01
			sta PortaDelta.lo,x
			lda #$ff
			adc #$00
			sta PortaDelta.hi,x
.0e.note.out
		rts
		
		
;//..........................................................
.0f
			sax
	rts


		
		
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
			tay
			and #$0f
		beq .8f.skip0
			dec a
			asl a
			asl a
			asl a
			asl a
			clc
			adc #low(VibratoWaveFormData)
			sta VbrPtr.lo,x
			lda #high(VibratoWaveFormData)
			adc #$00
			sta VbrPtr.hi,x
			;stz VbrIndex,x
.8f.skip0
			tya
			lsr a
			lsr a
			lsr a
			lsr a
		beq .8f.skip1
			;eor #$0f
			;inc a
			sta VbrSpeed,x
			;sta VbrCntr,x
.8f.skip1
			lda VbrFX,x
			and #$3f
			ora #$40
			sta VbrFX,x
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
			clc
			adc #$06
			tay
			bra .env_define
.92		;//Env 2 define
			sax
			txa
			clc
			adc #$0c
			tay

.env_define
		jsr .GetByteIncPtr
			sta EnvAttackStart,y
		jsr .GetByteIncPtr
			sta EnvAttackDelta,y
		jsr .GetByteIncPtr
			sta EnvLevel,y
		jsr .GetByteIncPtr
			sta EnvDecayDelta,y
		jmp .return

;//..........................................................
.93		;// Key Set (redundant)
			sax	
		jsr .GetByteIncPtr
			sta KeyHold,x
			sta KeyHoldInit,x
			stz KeyOn,x
			inc KeyOn,x
		jmp .return

;//..........................................................
.94		;// Key release
			sax	
			stz KeyHold,x
			lda KeyOn,x
			ora #$80
			sta KeyOn,x
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
			clc
			adc #$02
			sta TrkPtrStack.lo,x
			lda <TrkCrntPtr+1
			adc #$00
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
			and #$F0
			sta TrkPanVolLeft,x
			lda TrkPanVolMix,x
			asl a
			asl a
			asl a
			asl a
			sta TrkPanVolRight,x
 		jsr PanUpdateSub
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
			sta MasterPanMix
			stz $800
			sta $801
			php
			and #$0f
			sta MasterPanLeft
			sta MasterPanLeftSet
			pla
			lsr a
			lsr a
			lsr a
			lsr a
			sta MasterPanRight
			sta MasterPanRightSet
			stz MasterFadeCntr		;stop any fade routine (also used to init it)
			
		jmp .return
		
;//..........................................................
.9f		;// Fine Volume slide up
			sax
		jsr .GetByteIncPtr
		jsr .do9f
		jmp .return

.do9f							
			sta TrkPanVolMix,x
			lda TrkDirVol,x
			sec
			sbc TrkPanVolMix,x
			bcs .9f_skip
			cla
.9f_skip
			sta TrkDirVol,x
		jsr PanUpdateSub
		rts

;//..........................................................
.a0		;// Fine Volume slide down
			sax
		jsr .GetByteIncPtr
		jsr .doa0
		jmp .return
		
.doa0
			sta TrkPanVolMix,x
			lda TrkDirVol,x
			clc
			adc TrkPanVolMix,x
			bcc .a0_skip
			lda #$ff
.a0_skip
			sta TrkDirVol,x
		jsr PanUpdateSub
		rts

;//..........................................................
.a1		;// Direct volume set
			sax
		jsr .GetByteIncPtr
			sta TrkDirVol,x
		jsr PanUpdateSub
		jmp .return

;//..........................................................
.a2		;// Direct volume mode on
			sax
			stz EnvMode,x			;//Env off
			stz KeyHold,x
			stz KeyOn,x
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

;//..........................................................
.a5		;// Rest Loop
			sax
		jsr .GetByteIncPtr
			sta TrkRestLoop,x
		jmp .return

;//..........................................................
.a6		;// Channel Volume slide ToDo
			sax
		jsr .GetByteIncPtr
		jmp .return

;//..........................................................
.a7		;// Portamento Delta Set
			sax
		jsr .GetByteIncPtr
			sta TrkPortaArg,x		
	jmp .return

;//..........................................................
.a8		;// Portamento Cont
			sax
			lda PortaFX,x
			and #$7f
			sta PortaFX,x
		jmp .return

;//..........................................................
.a9		;// Portamento Pause/Unpause
			sax
			lda PortaFX,x
			eor #$10								;<- pause/unpause auto mode
			sta PortaFX,x
		jmp .return

;//..........................................................
.aa		;// Portamento step
			sax
			lda PortaFX,x
			and #$0f								;<- step mode turns off auto mode/clears pause mode
			ora #$40							
			sta PortaFX,x
		jmp .return
;//..........................................................
.ab		;// Portamento Auto on
			sax
			lda PortaFX,x						;<- auto mode turns off step mode
			and #$1f
			ora #$a0
			sta PortaFX,x
		jmp .return
;//..........................................................
.ac		;// Portamento Off
			sax
			stz PortaFX,x
		jmp .return

;//..........................................................
.ad		;// Arpeggio setup
			sax
		jsr .GetByteIncPtr
			pha
			and #$0f
			sta Arp3,x
			pla
			lsr a
			lsr a
			lsr a
			lsr a
			sta Arp2,x
		jmp .return

;//..........................................................
.ae		;// Arpeggio mode 0=disable, 1=note trigger, 2=auto
			sax
		jsr .GetByteIncPtr
			cmp #$01
		bcs .ae.skip
			sta ArpMode,x
		bra .ae.out
.ae.skip
			cmp #$02
		bcs .ae.skip1
			ora #$10
			sta ArpMode,x
		bra .ae.out
.ae.skip1
			cmp #$03
		bcs .ae.error
			sta ArpMode,x
		bra .ae.out
.ae.error
			stz ArpMode,x
.ae.out
		jmp .return

;//..........................................................
.af		;// Combo1 Step
;			sax
;		jsr .GetByteIncPtr
;			tay
;			bit #$01					;<- step arpeggio
;		beq .af.next	
;			lda ArpMode,x
;			and #$7f
;			ora #$40
;			sta ArpMode,x
;			stz ArpCntr,x
;.af.next						
;			tya
;			bit #$02					;<- step keyrelease
;		beq .af.next1
;			stz KeyHold,x
;			lda KeyOn,x
;			ora #$80
;			sta KeyOn,x
;.af.next1						
;			tya
;			bit #$04					;<- step Portamento (previous mode.)
;		beq .af.next2
;			lda PortaFX,x
;			and #$0f								
;			ora #$40							
;			sta PortaFX,x
;.af.next2
;			tya
;			bit #$08					;<- step Portamento up (can be used to change direction)
;		beq .af.next3
;			cla
;		jsr .0e.up
;		
;		
;.af.next3
;			tya
;			bit #$10					;<- step Portamento down (can be used to change direction)
;		beq .af.next4
;			lda #$01
;		jsr .0e.down
		
;.af.next4
;			tya
;			bit #$20					
;		beq .af.next5


;.af.next5
		jmp .return

;//..........................................................
.b0		;// Portamento 3/2/1xy <- continue portamento, but with new delta value
			sax
		jmp .return

;//..........................................................
.b1		;// Step Arpeggio
			sax
		jsr ArpStepSub
		jmp .return

;//..........................................................
.b2		;// Call + length
			sax

			lda #$81
			sta TrkJsr,x

		jsr .GetByteIncPtr
			sta TrkCallLen,x
			lda <TrkCrntPtr
			clc
			adc #$02
			sta TrkPtrStack.lo,x
			lda <TrkCrntPtr+1
			adc #$00
			sta TrkPtrStack.hi,x
			jsr .change_ptr 
		jmp .trackread					

;//..........................................................
.b3		;//Key disable
			sax
			stz KeyOn,x
		jmp .return

;//..........................................................
.b4		;//Play Sample
			sax
		jsr .GetByteIncPtr
			tay
.b4_cont
			lda #$01
			sta <DDAon 
			lda SampleTBL.lo,y
			sta DDAptr
			lda SampleTBL.hi,y
			sta DDAptr+1
			lda SampleTBL.bnk,y
			sta DDAbank
		jmp .return					


;//..........................................................
.b5		;// Arpstep+KeyRelease
			sax
		jsr ArpStepSub
			stz KeyHold,x
			lda KeyOn,x
			ora #$80
			sta KeyOn,x
		jmp .return					

;//..........................................................
.b6		;//Portamento step down
			sax
			lda #$01
		jsr .0e.down
		jmp .return					

;//..........................................................
.b7		;// Prefix2
			sax
			lda #$02
			sta PrefixCnt
		jmp .return

;//..........................................................
.b8		;// Prefix3
			sax
			lda #$03
			sta PrefixCnt
		jmp .return

;//..........................................................
.b9		;// Prefix4
			sax
			lda #$04
			sta PrefixCnt
		jmp .return

;//..........................................................
.ba		;// Fine Volume slide down
			sax
			lda #$10
		jsr .doa0
		jmp .return

;//..........................................................
.bb		;// Vibrato step
			sax
		jmp .8f.skip1

;//..........................................................
.bc		;// DDA 0
			sax
			cly
		bra .b4_cont

;//..........................................................
.bd		;// DDA 1
			sax
			ldy #$01
		bra	.b4_cont

;//..........................................................
.be		;// DDA 2
			sax
			ldy #$02
		bra .b4_cont


	
	
;.......................................................................
VibratoWaveFormData:													 												 ;
																			 																 ;
;	.db	$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01	 ;
																			 																 ;
	.db	$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01	 ;
																			 																 ;
	.db	$00,$00,$00,$01,$01,$01,$02,$02,$02,$03,$03,$03,$03,$03,$03,$03	 ;
																			 																 ;
	.db	$00,$00,$01,$01,$02,$02,$03,$03,$04,$04,$04,$05,$05,$05,$05,$05	 ;
																			 																 ;
	.db	$00,$00,$01,$02,$03,$03,$04,$05,$05,$06,$06,$07,$07,$07,$07,$07	 ;
																			 																 ;
	.db	$00,$00,$01,$02,$03,$04,$05,$06,$07,$07,$08,$08,$09,$09,$09,$09	 ;
																			 																 ;
	.db	$00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$09,$0A,$0B,$0B,$0B,$0B	 ;
																			 																 ;
	.db	$00,$01,$02,$04,$05,$06,$07,$08,$09,$0A,$0B,$0C,$0C,$0D,$0D,$0D	 ;
																			 																 ;
	.db	$00,$01,$03,$04,$06,$07,$08,$0A,$0B,$0C,$0D,$0E,$0E,$0F,$0F,$0F	 ;
																			 																 ;
	.db	$00,$01,$03,$05,$06,$08,$09,$0B,$0C,$0D,$0E,$0F,$10,$11,$11,$11	 ;
																			 																 ;
	.db	$00,$01,$03,$05,$07,$09,$0B,$0C,$0E,$0F,$10,$11,$12,$13,$13,$13	 ;
																			 																 ;
	.db	$00,$02,$04,$06,$08,$0A,$0C,$0D,$0F,$10,$12,$13,$14,$14,$15,$15	 ;
																			 																 ;
	.db	$00,$02,$04,$06,$09,$0B,$0D,$0F,$10,$12,$13,$15,$16,$16,$17,$17	 ;
																			 																 ;
	.db	$00,$02,$04,$07,$09,$0C,$0E,$10,$12,$14,$15,$16,$17,$18,$19,$19	 ;
																			 																 ;
	.db	$00,$02,$05,$08,$0A,$0D,$0F,$11,$13,$15,$17,$18,$19,$1A,$1B,$1B	 ;
																			 																 ;
	.db	$00,$02,$05,$08,$0B,$0E,$10,$12,$15,$17,$18,$1A,$1B,$1C,$1D,$1D	 ;
;.......................................................................
	
	
	
	


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
			lda <vdc_reg
			sta $0000
			stz <__vblank
			lda <__BXR
			sta <__wBXR

		bra .exit 

						
.hsync	
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
	  .dw DDATimer
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
			track.mcp:		.ds 1
			TrkCrntPtr:		.ds 2
			VbrPtr:				.ds 2
			DDAon:				.ds 1
			DDAbank:			.ds 1
			DDAptr:				.ds 1

	.bss
			
			__DMA:							.ds 8
			TileBuffer:					.ds 32
			VColor.lo:					.ds 240
			VColor.hi:					.ds 240
			KeyHold:						.ds 6
			KeyHoldInit:				.ds 6
			KeyOn:							.ds 6
			TrkDirVol:					.ds 6
;			Vibrato:						.ds 6
			VbrPtr.lo:					.ds 6
			VbrPtr.hi:					.ds 6
			VbrSpeed:						.ds 6
			VbrCntr:						.ds 6
			VbrIndex:						.ds 6
			TrkVbrDelta.lo:			.ds 6
			TrkVbrDelta.hi:			.ds 6
			VbrFX:							.ds 6
			TrackPanVolCurrent:	.ds 6
			TrackPanVolUpdate:	.ds 6
			EnvAttackStart:			.ds 6*4
			EnvAttackDelta:			.ds 6*4
			EnvDecayDelta:			.ds 6*4
			EnvLevel:						.ds 6*4
			TrkPtrStack:				.ds 6
			TrkJsr:							.ds 6
			TrkCallLen:					.ds 6
			TrkUpdate:					.ds 6
			TrkFX:							.ds 6
			EnvMode:						.ds 6
			EnvVol:							.ds 6
			BaseVol:						.ds 6
			TrkPanVolLeft:			.ds 6
			TrkPanVolRight:			.ds 6
			TrkPanVolMix:				.ds 6
			MasterPanLeftSet:		.ds 1
			MasterPanRightSet:	.ds 1
			MasterPanLeft:			.ds 1
			MasterPanRight:			.ds 1
			MasterPanMix:				.ds 1
			EngineStat:					.ds 1
			tempo:							.ds 1
			TickLen:						.ds 1
			TrkPtrStack.lo:			.ds 6
			TrkPtrStack.hi:			.ds 6
			PrefixCnt:					.ds 1
			TrkRestLoop:				.ds 6
			TrkUpdate2:					.ds 6
			MasterFadeDir:			.ds 1
			MasterFadeDelta:		.ds 1
			MasterFadeDelay:		.ds 1
			MasterFadeCntr:			.ds 1
			TrkCurrentNote:			.ds 6
			PortaDelta.lo:			.ds 6
			PortaDelta.hi:			.ds 6
			PortaNote.lo:				.ds 6
			PortaNote.hi:				.ds 6
			TrkPortaCntr:				.ds 6
			TrkPortaNote:				.ds 6
			PortaFX:						.ds 6
			TrkPortaArg:				.ds 6
			TrkNoteTemp:				.ds 6
			TrkPeriodLoad.lo:		.ds 6
			TrkPeriodLoad.hi:		.ds 6
			ArpCntr:						.ds 6
			ArpMode:						.ds 6
			ArpCntr2:						.ds 6
			Arp1:								.ds 6
			Arp2:								.ds 6
			Arp3:								.ds 6
			TrueNote:						.ds 6
			VolSlideArg:				.ds 6
			VolSlideMode:				.ds 6
						
			
			
			MCP:						.ds 1
			
			SongDepack:					.ds SongSize
			
			;EndBss:					.ds 1
			
			
			

		

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

;//##################################################################################################
;

	.code
	.bank $01
	.org SongDepack

TestSong:

;//........................................................................................
;lead0
SongTrack0:
			.db Prefix.,$0a,  TickSet.,$07,  BaseVol.,$ff,  PanVol.,$12,  WFset.,$02,  MasterPan.,$ff
			.db EnvDef1.,$fe,$01,$a8,$01,  EnvSelect.,$01,  EnvMode.,  KeySet.,$01
			.db DirVolSet.,$ff

.songoff0
.songoff5
;			.db Rest.
;			.db Jump.,low(.songoff5),high(.songoff5)


			.db RestLoop., 63
			.db Call., low(LeadIntro0),high(LeadIntro0)
			.db Prefix2.,  PortaAutoOff.,  Call., low(Lead0),high(Lead0)
			.db Call.,low(Lead1),high(Lead1)
			.db Prefix2., ArpMode.,ArpDisable., Call.,low(P4C0),high(P4C0)
			.db Call.,low(P5C0),high(P5C0)
			.db Call.,low(P6C0),high(P6C0)
			.db Call.,low(P7C0),high(P7C0)

			;pattern 8
			.db Prefix2., PanVol.,$44, Call., low(Lead0),high(Lead0)
			.db Call., low(Lead0),high(Lead0)
			.db Call., low(Lead0),high(Lead0)
			.db Call., low(Lead0),high(Lead0)
			.db CallLen.,41, low(Lead0),high(Lead0), Call.,low(PCC0),high(PCC0)

			;pattern D
			.db Prefix3.,  PortaAutoOff., PanVol.,$12
			.db Call., low(Lead0),high(Lead0)
			.db CallLen.,41, low(Lead1),high(Lead1), RestLoop.,22
			
			.db Call., low(PEC5), high(PEC5)


			.db Prefix4., DirVolSet.,$ff, PortaAutoOff.,  ArpMode.,ArpDisable., Jump.,low(.songoff5),high(.songoff5)



;//........................................................................................
;lead1/echo/drum
SongTrack1:
			.db Prefix.,$08, BaseVol.,$ff, PanVol.,$56, WFset.,$02, MasterPan.,$ff
			.db EnvDef1., $fe,$01,$a8,$01, EnvSelect., $01,  EnvMode.
			.db DirVolSet.,$ff

			

.songoff1

.songoff5
;			.db Rest.
;			.db Jump.,low(.songoff5),high(.songoff5)
			
			

			
	;			.db Prefix.,02,  KeySet.,$01
	;			.db RestLoop., 63
	;			.db RestLoop., 63
			.db Rest., Rest., Rest., Rest.
			.db Prefix.,05, PanVol.,$89,  KeySet.,$01,  PortaSet.,$32,  PortaAutoOn.
			.db Call., low(Public0),high(Public0)
			.db CallLen.,60, low(Public0),high(Public0)

			.db Prefix2., PanVol.,$56,  Call., low(P2C5),high(P2C5)
			.db CallLen.,34, low(P2C5),high(P2C5), Prefix2., PanVol.,$22, Call., low(P3C5),high(P3C5)
			.db Prefix2., PanVol.,$56 
			.db RestLoop.,3
			.db Prefix2., ArpMode.,ArpDisable., Call.,low(P4C0),high(P4C0)
			.db Call.,low(P5C0),high(P5C0)
			.db Call.,low(P6C0),high(P6C0)
			.db CallLen.,60,low(P7C0),high(P7C0)

			;pattern8
			.db DirVolSet.,$ff, Rest., Rest., Prefix3., KeySet.,$01, PanVol.,$88, Call., low(Lead0),high(Lead0)
			.db Call., low(Lead0),high(Lead0)
			.db Call., low(Lead0),high(Lead0)
			.db Call., low(Lead0),high(Lead0)
			.db CallLen.,41, low(Lead0),high(Lead0), RestLoop.,19

			;pattern D
			.db Prefix3., ArpMode.,ArpDisable., PanVol.,$56,  Call.,low(P2C5),high(P2C5)
			.db RestLoop.,2, CallLen.,41, low(Lead1),high(Lead1), RestLoop.,19


			.db DirVolSet., $ff, RestLoop.,62

			.db Prefix4., DirVolSet.,$ff, PortaAutoOff.,  ArpMode.,ArpDisable., Jump.,low(.songoff5),high(.songoff5)



;//........................................................................................
;Bass
SongTrack2: 
			.db Prefix.,$08, BaseVol.,$ff, PanVol.,$11, WFset.,$02, MasterPan.,$ff
			.db EnvDef1., $fe,$01,$b8,$02, EnvSelect., $01,  EnvMode.
			.db DirVolSet.,$ff


.songoff2
;			.db Rest.
;			.db Jump.,low(.songoff2),high(.songoff2)
			
		;Pattern 0
			.db RestLoop., 63
		;Pattern 1
			.db Prefix3.,  PortaSet.,$03,  PortaAutoOn.
			.db Call.,low(Bass0),high(Bass0)
		;Pattern 2
			.db Prefix2.,  PortaAutoOff.
			.db Call.,low(Bass1),high(Bass1)
		;Pattern 3
			.db Call.,low(Bass2),high(Bass2)
		;Pattern 4
			.db Call.,low(Bass3),high(Bass3)
		;Pattern 5
			.db Call.,low(Bass3),high(Bass3)
		;Pattern 6
			.db Call.,low(Bass3),high(Bass3)
		;Pattern 7
			.db CallLen.,56,low(Bass3),high(Bass3),  Call.,low(P7C3),high(P7C3)
			
		;pattern8
			.db Prefix.,06, WFset.,01, ArpSet.,$CC,  KeySet.,$01,  ArpMode.,ArpNoteTrig., PanVol.,$55
			.db Call., low(P8C2),high(P8C2)
		;pattern9
			.db Prefix2., PanVol.,$33, Call., low(P8C2),high(P8C2)
		;patternA
			.db Call., low(P8C2),high(P8C2)
		;patternB
	 		.db Call., low(P8C2),high(P8C2)
		;patternC
	 		.db Call., low(P8C2),high(P8C2)

		;patternD
			.db Prefix4., WFset.,02, ArpMode.,ArpDisable., PanVol.,$11, Call.,low(Bass1),high(Bass1)
		;patternE
			.db Call.,low(Bass2),high(Bass2)
		;patternF
			.db Call., low(PEC3), high(PEC3)

			.db Prefix4., DirVolSet.,$ff, PortaAutoOff.,  ArpMode.,ArpDisable., Jump.,low(.songoff2),high(.songoff2)




;//........................................................................................
;
SongTrack3:
			.db Prefix.,$08, BaseVol.,$FF, PanVol.,$23, WFset.,$02, MasterPan.,$ff
			.db EnvDef1., $a0,$a0,$f3,$08, EnvSelect., $01, EnvMode.
			.db DirVolSet.,$ff



.songoff3
;			.db Rest.
;			.db Jump.,low(.songoff3),high(.songoff3)

			.db Prefix3.,  PortaSet.,$32,  PortaAutoOn.
			.db Call., low(Public0),high(Public0)
			.db Call., low(Public0),high(Public0)
			.db Call., low(Public0),high(Public0)
			.db Call., low(P3C3),high(P3C3)
			.db Call., low(Public0),high(Public0)
			.db Call., low(Public0),high(Public0)
			.db Call., low(Public0),high(Public0)
			.db Call., low(Public0),high(Public0)

			;pattern8
			.db Prefix4., EnvDef1.,$fe,$01,$b8,$02
			.db KeySet.,01, PanVol.,$33,  Call.,low(P2C5),high(P2C5)
			.db Prefix3., PanVol.,$11,  ArpMode.,ArpDisable.,  Call.,low(P9C4),high(P9C4)
			.db Call., low(PAC4),high(PAC4)
			.db Call., low(PBC4),high(PBC4)
			.db Call., low(PCC4),high(PCC4)


			.db Prefix.,06,  PortaSet.,$32,  PortaAutoOn., PanVol.,$11, EnvDef1.,$a0,$a0,$f3,$08
			.db KeyDisable., Call., low(Public0),high(Public0)
			.db CallLen.,31, low(Public0),high(Public0), RestLoop.,32

			.db DirVolSet.,$ff,  RestLoop., 62

			.db Prefix4., DirVolSet.,$ff, PortaAutoOff.,  ArpMode.,ArpDisable., Jump.,low(.songoff3),high(.songoff3)




;//........................................................................................
; Echo/background
SongTrack4:
			.db Prefix.,$08, BaseVol.,$ff, PanVol.,$45, WFset.,$02, MasterPan.,$ff
			.db EnvDef1., $80, $80,$a0,$1f, EnvSelect., $01, DirVolMode.
			.db DirVolSet.,$ff

.songoff5
;			.db Rest.
;			.db Jump.,low(.songoff5),high(.songoff5)

.songoff4


			.db Rest., Rest.
			.db Prefix3.,  PortaSet.,$32,  PortaAutoOn.
			.db Call., low(Public0),high(Public0)
			.db Call., low(Public0),high(Public0)
			.db Call., low(Public0),high(Public0)
			.db CallLen.,62, low(Public0),high(Public0)
			.db Prefix.,06, PanVol.,$22
			.db EnvDef1.,$fe,$01,$b8,$02, ArpSet.,$CC,  KeySet.,$01,  ArpMode.,ArpNoteTrig.
			.db Call., low(P4C2),high(P4C2)
			.db Call., low(P4C2),high(P4C2)
			.db Call., low(P4C2),high(P4C2)
			.db Call., low(P4C2),high(P4C2)

				;pattern8
			.db Prefix4., EnvDef1., $fe,$01,$b8,$02
			.db KeySet.,01, PanVol.,$77
			.db CallLen.,64, low(P2C5),high(P2C5)
			.db Prefix3., PanVol.,$55,  ArpMode.,ArpDisable.,  Call.,low(P9C4_pre),high(P9C4_pre)
			.db Call., low(PAC4),high(PAC4)
			.db Call., low(PBC4),high(PBC4)
			.db CallLen.,60, low(PCC4),high(PCC4)

			.db Rest., Rest., Rest.
			.db Prefix4.,  PortaSet.,$32,  PortaAutoOn., PanVol.,$45
			.db Call., low(Public0),high(Public0)
			.db CallLen.,61, low(Public0),high(Public0)


			.db DirVolSet., $ff, RestLoop.,62
			.db Prefix4., DirVolSet.,$ff, PortaAutoOff.,  ArpMode.,ArpDisable., Jump.,low(.songoff5),high(.songoff5)







;//........................................................................................
; Echo/Drums
SongTrack5:
			.db Prefix.,08, BaseVol.,$FF, PanVol.,$00, WFset.,$02, MasterPan.,$ff
			.db EnvDef1., $80, $80,$a0,$1f, EnvSelect., $01, DirVolMode.
			.db DirVolSet.,$ff


.songoff5
;			.db Rest.
;			.db Jump.,low(.songoff5),high(.songoff5)


	;			.db Rest., Rest., Rest., Rest.
	
	;			.db Prefix., $03,  PortaSet.,$32,  PortaAutoOn.
	;			.db Call., low(Public0),high(Public0)
	;			.db CallLen.,60, low(Public0),high(Public0)
	; 			.db DirVolSet., $ff, RestLoop.,62

				;pattern 0
			.db RestLoop.,63
				;pattern 1
 			.db Call.,low(DDA1),high(DDA1)
				;pattern 2
 			.db Call.,low(DDA2),high(DDA2)
				;pattern 3
 			.db CallLen.,34,low(DDA2),high(DDA2),  Call.,low(DDA3),high(DDA3)
				;pattern 4
 			.db CallLen.,28,low(DDA2),high(DDA2), Call.,low(DDA4_0),high(DDA4_0)
 			  .db CallLen.,23,low(DDA4_2),high(DDA4_2), Call.,low(DDA4_1),high(DDA4_1)
				;pattern 5
 			.db CallLen.,28,low(DDA2),high(DDA2), Call.,low(DDA4_0),high(DDA4_0)
 			  .db CallLen.,23,low(DDA4_2),high(DDA4_2), Call.,low(DDA4_1),high(DDA4_1)
				;pattern 6
 			.db CallLen.,28,low(DDA2),high(DDA2), Call.,low(DDA4_0),high(DDA4_0)
 			  .db CallLen.,23,low(DDA4_2),high(DDA4_2), Call.,low(DDA4_1),high(DDA4_1)
				;pattern 7
 			.db CallLen.,28,low(DDA2),high(DDA2), Call.,low(DDA4_0),high(DDA4_0)
 			  .db CallLen.,21,low(DDA4_2),high(DDA4_2), Call.,low(DDA7_0),high(DDA7_0)
				;pattern 8
 			.db Call.,low(DDA8),high(DDA8)
				;pattern 9
 			.db Call.,low(DDA8),high(DDA8)
				;pattern A
 			.db Call.,low(DDA8),high(DDA8)
				;pattern B
 			.db Call.,low(DDA8),high(DDA8)
				;pattern C
 			.db CallLen.,50,low(DDA8),high(DDA8), Call.,low(DDAC_0),high(DDAC_0)
				;pattern D
 			.db Call.,low(DDA2),high(DDA2)
				;pattern E
 			.db CallLen.,34,low(DDA2),high(DDA2),  CallLen.,19,low(DDA3),high(DDA3)
 				.db RestLoop.,10
				;pattern F
 			.db RestLoop.,63

			.db Jump.,low(.songoff5),high(.songoff5)



;//........................................................................................
;Patterns


Public0:			
			.db F.4, Porta | PortaOff
			.db Gx4, Porta | PortaNote
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
			.db _Ax4
			.db _D.5
			.db _F.5
			.db _Ax5
			.db _D.6
			.db _Ax5
			.db _F.5
			.db _D.5
			.db _Ax4
			.db _D.5
			.db _F.5
			.db _Ax5
			.db _D.6
			.db _Ax5
			.db _F.5
			.db _D.5
			.db _F.4
			.db _C.5
			.db _F.5
			.db _C.6
			.db _F.6
			.db _C.6
			.db _F.5
			.db _C.5
			.db _F.4
			.db _C.5
			.db _F.5
			.db _C.6
			.db _F.6
			.db _C.6
			.db _F.5
			.db _C.5
			.db Return.

LeadIntro0:
			.db RestLoop.,58
			.db C.6, VolSet. | $F0
			.db Prefix3.,   PortaSet.,$0e,   PortaAutoOn.,  C.5, Porta | PortaNote
			.db RestLoop.,2
			.db Return.
			
			
Lead0:
			.db Prefix.,06,  ArpSet.,$CC, KeySet.,$01,  ArpMode., ArpNoteTrig.,  PortaSet.,$0e,   PortaStep.
			.db	_F.4
			.db Arp_Key.
			.db	_F.4
			.db	F.4, VolSlideDown. | $40	
			.db	_C.5
			.db	_C.5
			.db	_C.5
			.db Arp_Key.
			.db ArpStep. 
			.db ArpStep. 
			.db	F.4, VolSlideDown. | $80 
			.db F.4, VolSlideDown. | $40 
			.db C.5, VolSlideDown. | $60 
			.db	_F.4
			.db	_F.5
			.db Prefix2.,  FineSlideVolDown.,$20,  PortaStepDown. ;;Combo1 | VolSub | PortaSlideDown, $0e, $0e
			.db	_Dx5
			.db	F.5, VolSet. | $40  
			.db	_Dx5
			.db	_Dx5
			.db	_C.5
			.db	_C.5
			.db	_C.5
			.db Arp_Key.
			.db ArpStep. 
			.db ArpStep. 
			.db	_F.4
			.db	_F.4
			.db	_Dx5
			.db	_F.4
			.db	_F.5
			.db Prefix2.,  FineSlideVolDown.,$20,  PortaStepDown. ;;Combo1 | VolSub | PortaSlideDown, $0e, $0e
			.db	_D.5
			.db	F.5, VolSet. | $40 
			.db	_D.5
			.db	D.5, VolSlideDown. | $80  
			.db	_Ax4
			.db	_Ax4
			.db	_Ax4
			.db Arp_Key.
			.db	ArpStep. 
			.db	ArpStep. 
			.db	_D.5
			.db	_D.5
			.db	_D.5
			.db	_C.5
			.db	_Ax4
			.db	_Ax4
			.db	_C.5
			.db	_C.5
			.db	_C.5
			.db	_C.5
			.db	_C.5
			.db	_C.5
			.db	_C.5
			.db Arp_Key.
			.db	ArpStep. 
			.db ArpStep. 
			.db ArpStep. 
			.db ArpStep. 
			.db ArpStep. 
			.db ArpStep. 
			.db ArpStep. 
			.db ArpStep. 
			.db Return.

Lead1:
			.db Prefix.,06,  ArpSet.,$CC, KeySet.,$01,  ArpMode., ArpNoteTrig.,  PortaSet.,$0e,   PortaStep.
			.db	_F.4
			.db Arp_Key.
			.db	_F.4
			.db	F.4, VolSlideDown. | $40	
			.db	_C.5
			.db	_C.5
			.db	_C.5
			.db Arp_Key.
			.db ArpStep. 
			.db ArpStep. 
			.db	F.4, VolSlideDown. | $80 
			.db F.4, VolSlideDown. | $40 
			.db C.5, VolSlideDown. | $60 
			.db	_F.4
			.db	_F.5
			.db Prefix2.,  FineSlideVolDown.,$20,  PortaStepDown. ;;Combo1 | VolSub | PortaSlideDown, $0e, $0e
			.db	_Dx5
			.db	F.5, VolSet. | $40  
			.db	_Dx5
			.db	_Dx5
			.db	_C.5
			.db	_C.5
			.db	_C.5
			.db Arp_Key.
			.db ArpStep. 
			.db ArpStep. 
			.db	_F.4
			.db	_F.4
			.db	_Dx5
			.db	_F.4
			.db	_F.5
			.db Prefix2.,  FineSlideVolDown.,$20,  PortaStepDown. ;;Combo1 | VolSub | PortaSlideDown, $0e, $0e
			.db	_D.5
			.db	F.5, VolSet. | $40 
			.db	_D.5
			.db	D.5, VolSlideDown. | $80  
			.db	_Ax4
			.db	_Ax4
			.db	_Ax4
			.db Arp_Key.
			.db	ArpStep. 
			.db	ArpStep. 
			.db	RestLoop.,4
			.db	Prefix4.,  ArpMode., ArpDisable.,  PortaSet.,$09,  Ax5,Porta | PortaNote,  DirVolSet., $20
			.db	PortaStep.
			.db	PortaStep.
			.db	F.5, VolSet. | $60
			.db Gx5, VolSet. | $20
			.db	Ax5, VolSet. | $60
			.db	F.5, VolSet. | $20
			.db	Gx5, VolSet. | $60
			.db Dx5, VolSet. | $20
			.db	D.5, VolSet. | $20 
			.db Dx5, VolSet. | $60 
			.db Gx5, VolSet. | $20  
			.db D.5, VolSet. | $60  
			.db Prefix3.,  PortaSet.,$09,  Dx5,Porta | PortaNote,  DirVolSet., $20 
			.db PortaStep. 
			.db D.5, VolSet. | $20
			.db Rest.
			.db Return.

			
Bass0:
			.db F.1, $02
			.db FineSlideVolDown1.
			.db FineSlideVolDown1.
			.db FineSlideVolDown1.
			.db FineSlideVolDown1.
			.db FineSlideVolDown1.
			.db FineSlideVolDown1.
			.db FineSlideVolDown1.
			.db FineSlideVolDown1.
			.db FineSlideVolDown1.
			.db FineSlideVolDown1.
			.db FineSlideVolDown1.
			.db F.2, $02
			.db DirVolSet., $20
			.db Rest.
			.db Rest.
			.db Gx1, $02
			.db FineSlideVolDown1.
			.db FineSlideVolDown1.
			.db FineSlideVolDown1.
			.db FineSlideVolDown1.
			.db FineSlideVolDown1.
			.db FineSlideVolDown1.
			.db FineSlideVolDown1.
			.db FineSlideVolDown1.
			.db FineSlideVolDown1.
			.db FineSlideVolDown1.
			.db FineSlideVolDown1.
			.db Gx2, $02
			.db DirVolSet., $20
			.db Rest.
			.db Rest.
			.db Ax1, $02
			.db FineSlideVolDown1.
			.db FineSlideVolDown1.
			.db FineSlideVolDown1.
			.db FineSlideVolDown1.
			.db FineSlideVolDown1.
			.db FineSlideVolDown1.
			.db FineSlideVolDown1.
			.db FineSlideVolDown1.
			.db FineSlideVolDown1.
			.db FineSlideVolDown1.
			.db FineSlideVolDown1.
			.db Ax1, $02
			.db DirVolSet., $20
			.db Rest.
			.db Rest.
			.db Gx2, $02
			.db DirVolSet., $50
			.db Rest.
			.db G.2, $02
			.db DirVolSet., $50
			.db Rest.
			.db Dx2, $02
			.db RestLoop.,3
			.db F.2, Porta | PortaNote
			.db RestLoop., 3
			.db Return.

Bass1:
			.db Prefix3.,  KeySet.,$01, EnvMode.
			.db	_F.1 
			.db KeyRls.
			.db	_F.2
			.db KeyRls.
			.db	_F.1
			.db KeyRls.
			.db	_F.2
			.db KeyRls.
			.db	_F.1
			.db KeyRls.
			.db	_F.2
			.db KeyRls.
			.db	_F.1
			.db KeyRls.
			.db	_F.2
			.db KeyRls.
			.db	_Gx1
			.db KeyRls.
			.db	_Gx2
			.db KeyRls.
			.db	_Gx1
			.db KeyRls.
			.db	_Gx2
			.db KeyRls.
			.db	_Gx1
			.db KeyRls.
			.db	_Gx2
			.db KeyRls.
			.db	_Gx1
			.db KeyRls.
			.db	_Gx2
			.db KeyRls.
			.db	_Ax1
			.db KeyRls.
			.db	_Ax2
			.db KeyRls.
			.db	_Ax1
			.db KeyRls.
			.db	_Ax2
			.db KeyRls.
			.db	_Ax1
			.db KeyRls.
			.db	_Ax2
			.db KeyRls.
			.db	_Ax1
			.db KeyRls.
			.db	_Ax2
			.db KeyRls.
			.db	_F.1
			.db KeyRls.
			.db	_F.2
			.db KeyRls.
			.db	_F.1
			.db KeyRls.
			.db	_F.2
			.db KeyRls.
			.db	_F.1
			.db KeyRls.
			.db	_F.2
			.db KeyRls.
			.db	_F.1
			.db KeyRls.
			.db	_F.2
			.db KeyRls.
			.db Return.

Bass2:
			.db Prefix3.,  KeySet.,$01, EnvMode.
			.db	_F.1 
			.db KeyRls.
			.db	_F.2
			.db KeyRls.
			.db	_F.1
			.db KeyRls.
			.db	_F.2
			.db KeyRls.
			.db	_F.1
			.db KeyRls.
			.db	_F.2
			.db KeyRls.
			.db	_F.1
			.db KeyRls.
			.db	_F.2
			.db KeyRls.
			.db	_Gx1
			.db KeyRls.
			.db	_Gx2
			.db KeyRls.
			.db	_Gx1
			.db KeyRls.
			.db	_Gx2
			.db KeyRls.
			.db	_Gx1
			.db KeyRls.
			.db	_Gx2
			.db KeyRls.
			.db	_Gx1
			.db KeyRls.
			.db	_Gx2
			.db KeyRls.
			.db	_Ax1
			.db Rest.
			.db	Rest.
			.db Rest.
			.db	VbrDef.,$a1
			.db VibratoStep.
			.db	VibratoStep.
			.db VibratoStep.
			.db	VbrDef.,$02
			.db VibratoStep.
			.db	VibratoStep.
			.db VibratoStep.
			.db	VbrDef.,$03
			.db VibratoStep.
			.db	VbrDef.,$04
			.db VibratoStep.
			.db	Prefix2.,  PortaSet.,$1f,  Ax2,Porta | PortaNote
			.db PortaStep.
			.db	_Ax1
			.db Rest.
			.db	_Ax2
			.db _Ax1
			.db	Rest.
			.db _Ax2
			.db	_Ax1
			.db Rest.
			.db	_Ax2
			.db _Ax1
			.db	_Dx3
			.db VbrDef.,$a1
			.db	VbrDef.,$02
			.db VbrDef.,$03
			.db Return.


Bass3:
			.db Prefix3.,  KeySet.,$01, EnvMode.
			.db	_F.1 
			.db KeyRls.
			.db	_F.2
			.db KeyRls.
			.db	_F.3
			.db KeyRls.
			.db	_F.2
			.db KeyRls.
			.db	_F.1
			.db KeyRls.
			.db	_F.3
			.db KeyRls.
			.db	_F.1
			.db KeyRls.
			.db	_F.3
			.db KeyRls.
			.db	_Gx1
			.db KeyRls.
			.db	_Gx2
			.db KeyRls.
			.db	_Gx3
			.db KeyRls.
			.db	_Gx2
			.db KeyRls.
			.db	_Gx1
			.db KeyRls.
			.db	_Gx3
			.db KeyRls.
			.db	_Gx1
			.db KeyRls.
			.db	_Gx3
			.db KeyRls.
			.db	_Ax1
			.db KeyRls.
			.db	_Ax2
			.db KeyRls.
			.db	_Ax3
			.db KeyRls.
			.db	_Ax2
			.db KeyRls.
			.db	_Ax1
			.db KeyRls.
			.db	_Ax3
			.db KeyRls.
			.db	_Ax1
			.db KeyRls.
			.db	_Ax3
			.db KeyRls.
			.db	_F.1
			.db KeyRls.
			.db	_F.2
			.db KeyRls.
			.db	_F.1
			.db KeyRls.
			.db	_F.2
			.db KeyRls.
			.db	_F.1
			.db KeyRls.
			.db	_F.2
			.db KeyRls.
			.db	_F.1
			.db KeyRls.
			.db	_F.2
			.db KeyRls.
			.db Return.
			
			
P4C0:
			.db _C.5
			.db VbrDef.,$a1
			.db Prefix2.,  DirVolSet.,$40,  VibratoStep.
			.db VbrDef.,$02
			.db _F.5
			.db Rest.
			.db VbrDef.,$a1
			.db VbrDef.,$02
			.db VbrDef.,$03
			.db VbrDef.,$04
			.db _C.6
			.db VbrDef.,$a1
			.db Prefix2.,  DirVolSet.,$40,  VbrDef.,$02
			.db VbrDef.,$03
			.db Prefix2.,  PortaSet.,$07,  F.6,Porta | PortaNote
			.db PortaStep.
			.db PortaStep.
			.db PortaStep.
			.db VibratoStep.
			.db VbrDef.,$02
			.db _Dx6
			.db Gx5, VolSet. | $40
			.db _C.6
			.db Dx6, VolSet. | $40
			.db _Gx5
			.db Rest.
			.db _C.6
			.db Rest.
			.db VbrDef.,$a1
			.db VbrDef.,$02
			.db Prefix2.,  PortaSet.,$02,  Ax5,Porta | PortaNote
			.db PortaStep.
			.db _Ax5
			.db Rest.
			.db VbrDef.,$a1
			.db VibratoStep.
			.db VbrDef.,$02
			.db _Gx5
			.db F.5, VolSet. | $40
			.db Rest.
			.db _Ax5
			.db F.5, VolSet. | $40
			.db _Gx5
			.db _Ax5
			.db Gx5, VolSet. | $40
			.db _Gx5
			.db _Ax5
			.db Gx5, VolSet. | $40
			.db _C.6
			.db VbrDef.,$a1
			.db VbrDef.,$02
			.db Dx6, VolSet. | $40
			.db Prefix2.,  PortaSet.,$03,  F.6,Porta | PortaNote
			.db PortaStep.
			.db PortaStep.
			.db PortaStep.
			.db PortaStep.
			.db VbrDef.,$a1
			.db VibratoStep.
			.db VbrDef.,$02
			.db Prefix2.,  PortaSet.,$0f,  F.5,Porta | PortaNote
			.db PortaStep.
			.db PortaStep.
			.db PortaStep.

			.db Return.

P5C0:
			.db _F.5
			.db VbrDef.,$a1
			.db Prefix2.,  DirVolSet.,$40,  VibratoStep.
			.db _Dx5
			.db VbrDef.,$a1
			.db VbrDef.,$02
			.db _F.5
			.db VbrDef.,$a1
			.db VbrDef.,$02
			.db VbrDef.,$03
			.db Prefix2.,  PortaSet.,$07,  Gx5,Porta | PortaNote
			.db PortaStep.
			.db PortaStep.
			.db PortaStep.
			.db Prefix2., _F.5,   VibratoStep.
			.db VbrDef.,$02
			.db _Dx5
			.db F.5, VolSet. | $40
			.db _C.5
			.db Dx5, VolSet. | $40
			.db _F.5
			.db Rest.
			.db _Dx5
			.db Rest.
			.db VbrDef.,$a1
			.db VibratoStep.
			.db _Gx5
			.db Rest.
			.db Prefix2.,  PortaSet.,$0f,  Ax5,Porta | PortaNote
			.db PortaStep.
			.db VbrDef.,$a1
			.db VibratoStep.
			.db _C.6
			.db VbrDef.,$02
			.db _Ax5
			.db Gx5, VolSet. | $40
			.db _F.5
			.db _Gx5
			.db F.5, VolSet. | $40
			.db _Ax5
			.db _C.6
			.db Ax5, VolSet. | $40
			.db _Ax5
			.db _C.6
			.db Ax5, VolSet. | $40
			.db _Dx6
			.db VbrDef.,$a1
			.db VbrDef.,$02
			.db Prefix2.,  PortaSet.,$03,  F.6,Porta | PortaNote
			.db PortaStep.
			.db PortaStep.
			.db PortaStep.
			.db PortaStep.
			.db VbrDef.,$a1
			.db VibratoStep.
			.db VbrDef.,$02
			.db VibratoStep.
			.db VibratoStep.
			.db Prefix2.,  PortaSet.,$0f,  F.4,Porta | PortaNote
			.db PortaStep.
			.db PortaStep.
			.db PortaStep.
			.db PortaStep.
			.db PortaStep.

			.db Return.

P6C0:
			.db _F.4
			.db VbrDef.,$a1
			.db VibratoStep.
			.db VbrDef.,$02
			.db Prefix2.,  PortaSet.,$09,  C.5,Porta | PortaNote
			.db PortaStep.
			.db PortaStep.
			.db VbrDef.,$a1
			.db VbrDef.,$02
			.db VbrDef.,$03
			.db _F.4
			.db _F.4
			.db _C.5
			.db F.4, VolSet. | $40
			.db _F.5
			.db C.5, VolSet. | $40
			.db _Dx5
			.db Rest.
			.db VbrDef.,$a1
			.db VbrDef.,$02
			.db Prefix2.,  PortaSet.,$09,  C.5,Porta | PortaNote
			.db PortaStep.
			.db PortaStep.
			.db VbrDef.,$a1
			.db VibratoStep.
			.db VbrDef.,$02
			.db _F.4
			.db _F.4
			.db _Dx5
			.db F.4, VolSet. | $40
			.db _F.5
			.db Dx5, VolSet. | $40
			.db _D.5
			.db F.5, VolSet. | $40
			.db _D.5
			.db Prefix2.,  PortaSet.,$05,  Ax4,Porta | PortaNote
			.db PortaStep.
			.db PortaStep.
			.db VbrDef.,$a1
			.db VibratoStep.
			.db VbrDef.,$02
			.db VibratoStep.
			.db _D.5
			.db Rest.
			.db Ax4, VolSet. | $40
			.db _C.5
			.db _Ax4
			.db Rest.
			.db Prefix2.,  PortaSet.,$03,  C.5,Porta | PortaNote
			.db PortaStep.
			.db PortaStep.
			.db PortaStep.
			.db VbrDef.,$a1
			.db VibratoStep.
			.db VibratoStep.
			.db VbrDef.,$02
			.db VibratoStep.
			.db VibratoStep.
			.db VbrDef.,$03
			.db VibratoStep.
			.db VibratoStep.
			.db VbrDef.,$04
			.db VibratoStep.
			.db VibratoStep.
			.db Return.


P7C0:
			.db _F.4
			.db VbrDef.,$a1
			.db VibratoStep.
			.db VbrDef.,$02
			.db Prefix2.,  PortaSet.,$09,  C.5,Porta | PortaNote
			.db PortaStep.
			.db PortaStep.
			.db VbrDef.,$a1
			.db VbrDef.,$02
			.db VbrDef.,$03
			.db _F.4
			.db _F.4
			.db _C.5
			.db F.4, VolSet. | $40
			.db _F.5
			.db C.5, VolSet. | $40
			.db _Gx5
			.db Rest.
			.db VbrDef.,$a1
			.db VbrDef.,$02
			.db Prefix2.,  PortaSet.,$09,  Ax5,Porta | PortaNote
			.db PortaStep.
			.db PortaStep.
			.db VbrDef.,$a1
			.db VibratoStep.
			.db VbrDef.,$02
			.db _Dx6
			.db _Dx6
			.db _C.6
			.db Dx6, VolSet. | $40
			.db _Gx5
			.db Dx5, VolSet. | $40
			.db _Ax5
			.db C.6, VolSet. | $40
			.db _Dx6
			.db Prefix2.,  PortaSet.,$05,  F.6,Porta | PortaNote
			.db PortaStep.
			.db PortaStep.
			.db VbrDef.,$a1
			.db VibratoStep.
			.db VbrDef.,$02
			.db _F.6
			.db Rest.
			.db Dx6, VolSet. | $40
			.db _C.6
			.db _Dx6
			.db Rest.
			.db Prefix2.,  PortaSet.,$09,  C.6,Porta | PortaNote
			.db PortaStep.
			.db PortaStep.
			.db PortaStep.
			.db VbrDef.,$a1
			.db VibratoStep.
			.db VibratoStep.
			.db VbrDef.,$02
			.db VibratoStep.
			.db VibratoStep.
			.db VbrDef.,$03
			.db VibratoStep.
			.db VibratoStep.
			.db VbrDef.,$04
			.db VibratoStep.
			.db VibratoStep.
			.db VibratoStep.
			.db Return.

P4C2:
			.db Prefix4.,  ArpSet.,$CC,  KeySet.,$01,  ArpMode.,ArpNoteTrig.
			.db	C.5, StepArp.
			.db Prefix2.,  ArpStep.,  DirVolSet.,$40
			.db DirVolSet.,$ff
			.db	C.5, StepArp.
			.db Prefix2.,  ArpStep.,  DirVolSet.,$40
P4C2_1:
			.db DirVolSet.,$ff
			.db RestLoop.,$09
			.db	C.5, StepArp.
			.db Prefix2.,  ArpStep.,  DirVolSet.,$40
			.db DirVolSet.,$ff
			.db	C.5, StepArp.
			.db Prefix2.,  ArpStep.,  DirVolSet.,$40
			.db DirVolSet.,$ff
			.db RestLoop.,$09
			.db	Ax4, StepArp.
			.db Prefix2.,  ArpStep.,  DirVolSet.,$40
			.db DirVolSet.,$ff
			.db	Ax4, StepArp.
			.db Prefix2.,  ArpStep.,  DirVolSet.,$40
			.db DirVolSet.,$ff
			.db RestLoop.,$09
			.db	F.5, StepArp.
			.db Prefix2.,  ArpStep.,  DirVolSet.,$40
			.db DirVolSet.,$ff
			.db	F.5, StepArp.
			.db Prefix2.,  ArpStep.,  DirVolSet.,$40
			.db DirVolSet.,$ff
			.db RestLoop.,$09
			.db Return.

P2C5_pre:
			.db Prefix4.,  ArpSet.,$CC,  KeySet.,$01,  ArpMode.,ArpNoteTrig.
			.db Arp_Key.
			.db _Dx5
			.db Arp_Key.
P2C5:
			.db Prefix4.,  ArpSet.,$CC,  KeySet.,$01,  ArpMode.,ArpNoteTrig.
			.db _F.4
			.db Arp_Key.
			.db _Gx4
			.db Arp_Key.
			.db _C.4
			.db Arp_Key.
			.db _Dx4
			.db Arp_Key.
			.db _F.4
			.db Arp_Key.
			.db _C.5
			.db Arp_Key.
			.db _Gx4
			.db Arp_Key.
			.db _Dx5
			.db Arp_Key.
			.db _Dx4
			.db Arp_Key.
			.db _Gx4
			.db Arp_Key.
			.db _C.4
			.db Arp_Key.
			.db _Dx4
			.db Arp_Key.
			.db _Gx4
			.db Arp_Key.
			.db _C.5
			.db Arp_Key.
			.db _Dx5
			.db Arp_Key.
			.db _Gx4
			.db Arp_Key.
			.db _D.4
			.db Arp_Key.
			.db _Ax4
			.db Arp_Key.
			.db _Ax3
			.db Arp_Key.
			.db _D.4
			.db Arp_Key.
			.db _F.4
			.db Arp_Key.
			.db _Ax4
			.db Arp_Key.
			.db _D.5
			.db Arp_Key.
			.db _Ax4
			.db Arp_Key.
			.db _F.4
			.db Arp_Key.
			.db _Gx4
			.db Arp_Key.
			.db _C.4
			.db Arp_Key.
			.db _Dx4
			.db Arp_Key.
			.db _F.4
			.db Arp_Key.
			.db _C.5
			.db Arp_Key.
			.db _Gx4
			.db Arp_Key.
			.db _Dx5
			.db Arp_Key.
			.db Return.			

P3C3:
			.db F.4, Porta | PortaOff
			.db Gx4, Porta | PortaNote
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
			.db F.4, VolSet. | $10
			.db C.5, VolSet. | $10
			.db Dx5, VolSet. | $20
			.db Gx5, VolSet. | $20
			.db C.6, VolSet. | $20
			.db Gx5, VolSet. | $30
			.db Dx5, VolSet. | $30
			.db C.5, VolSet. | $30
			.db Gx4, VolSet. | $30
			.db C.5, VolSet. | $40
			.db Dx5, VolSet. | $40
			.db Gx5, VolSet. | $40
			.db C.6, VolSet. | $50
			.db Gx5, VolSet. | $50
			.db Dx5, VolSet. | $60
			.db C.5, VolSet. | $60
			.db Ax4, VolSet. | $70
			.db D.5, VolSet. | $70
			.db F.5, VolSet. | $80
			.db Ax5, VolSet. | $80
			.db D.6, VolSet. | $a0
			.db Ax5, VolSet. | $a0
			.db F.5, VolSet. | $b0
			.db D.5, VolSet. | $f0
			.db Ax4, VolSet. | $f0
			.db DirVolSet., $ff
			.db RestLoop., 21
			.db Return.


P3C5:
			.db Prefix4.,  ArpMode.,ArpDisable., PortaSet.,$0f,  F.5,Porta | PortaNote, DirVolSet.,$28
			.db PortaStep.
			.db PortaStep.
			.db PortaStep.
			.db PortaStep.
			.db VbrDef.,$a1
			.db VbrDef.,$02
			.db VbrDef.,$03
			.db VbrDef.,$04
			.db VbrDef.,$05
			.db Prefix2.,  PortaSet.,$0f,  Ax5,Porta | PortaNote
			.db PortaStep.
			.db PortaStep.
			.db F.5, VolSet. | $40
			.db _Gx5
			.db Ax5, VolSet. | $40
			.db _F.5
			.db Gx5, VolSet. | $40
			.db _Dx5
			.db _D.5
			.db Dx5, VolSet. | $40
			.db _Gx5
			.db D.5, VolSet. | $40
			.db Prefix2.,  PortaSet.,$09,  Dx5,Porta | PortaNote
			.db PortaStep.
			.db _D.5
			.db VbrDef.,$a1
			.db VbrDef.,$02
			.db _Ax4
			.db VbrDef.,$a1
			.db Return.
			
P7C3:
			.db Prefix2.,  PortaSet.,$0f,  F.1,Porta | PortaNote
			.db PortaStep.
			.db PortaStep.
			.db PortaStep.
			.db PortaStep.
			.db Prefix2.,  FineSlideVolDown1.,  PortaStep.
			.db Prefix2.,  FineSlideVolDown1.,  PortaStep.
			.db Prefix2.,  FineSlideVolDown1.,  PortaStep.
			.db Return.

P8C2:
			.db Prefix.,05,  ArpSet.,$CC,  KeySet.,$01,  ArpMode.,ArpNoteTrig.
			.db	C.5, StepArp., DirVolSet.,$10
			.db Prefix2.,  ArpStep.,  DirVolSet.,$40
			.db DirVolSet.,$ff
			.db	Prefix2., C.5, StepArp., DirVolSet.,$10
			.db Prefix2.,  ArpStep.,  DirVolSet.,$40
			.db DirVolSet.,$ff
			.db RestLoop.,$09
			.db	Prefix2., C.5, StepArp., DirVolSet.,$10
			.db Prefix2.,  ArpStep.,  DirVolSet.,$40
			.db DirVolSet.,$ff
			.db	Prefix2., C.5, StepArp., DirVolSet.,$10
			.db Prefix2.,  ArpStep.,  DirVolSet.,$40
			.db DirVolSet.,$ff
			.db RestLoop.,$09
			.db	Prefix2., Ax4, StepArp.,  DirVolSet.,$10
			.db Prefix2.,  ArpStep.,  DirVolSet.,$40
			.db DirVolSet.,$ff
			.db	Prefix2., Ax4, StepArp.,  DirVolSet.,$10
			.db Prefix2.,  ArpStep.,  DirVolSet.,$40
			.db DirVolSet.,$ff
			.db RestLoop.,$09
			.db	Prefix2., F.5, StepArp.,  DirVolSet.,$10
			.db Prefix2.,  ArpStep.,  DirVolSet.,$40
			.db DirVolSet.,$ff
			.db	Prefix2., F.5, StepArp.,  DirVolSet.,$10
			.db Prefix2.,  ArpStep.,  DirVolSet.,$40
			.db DirVolSet.,$ff
			.db RestLoop.,$09
			.db Return.


P9C4_pre:
			.db RestLoop.,3 
P9C4:
			.db _C.5
			.db VbrDef.,$a1
			.db Prefix2., VibratoStep., DirVolSet., $40
			.db VbrDef.,$02
			.db _F.5
			.db Rest.
			.db VbrDef.,$a1
			.db VbrDef.,$02
			.db VbrDef.,$03
			.db VbrDef.,$04
			.db _C.6
			.db VbrDef.,$a1
			.db Prefix2., VbrDef.,$02, DirVolSet., $40
			.db VbrDef.,$03
			.db Prefix2.,  PortaSet.,$07,  Ax5,Porta | PortaNote
			.db PortaStep.
			.db PortaStep.
			.db PortaStep.
			.db VibratoStep.
			.db VbrDef.,$02
			.db _Gx5
			.db Rest.
			.db _F.5
			.db Rest.
			.db _Dx5
			.db Rest.
			.db _F.5
			.db Rest.
			.db VbrDef.,$a1
			.db VibratoStep.
			.db Prefix2.,  PortaSet.,$09,  Ax5,Porta | PortaNote
			.db PortaStep.
			.db _Ax5
			.db Rest.
			.db VbrDef.,$a1
			.db VibratoStep.
			.db VibratoStep.
			.db VbrDef.,$02
			.db VibratoStep.
			.db VbrDef.,$03
			.db _Gx5
			.db Rest.
			.db Ax5, VolSet. | $40
			.db Rest.
			.db _F.5
			.db Gx5, VolSet. | $40
			.db _Gx5
			.db F.5, VolSet. | $40
			.db _Ax5
			.db Gx5, VolSet. | $40
			.db _C.6
			.db Ax5, VolSet. | $40
			.db Prefix2.,  PortaSet.,$09,  C.6,Porta | PortaNote
			.db PortaStep.
			.db PortaStep.
			.db PortaStep.
			.db PortaStep.
			.db PortaStep.
			.db PortaStep.
			.db Rest.
			.db VbrDef.,$a1
			.db VibratoStep.
			.db VbrDef.,$02
			.db VibratoStep.
			.db Return.

PAC4:
			.db _F.6
			.db VbrDef.,$a1
			.db Prefix2., VibratoStep., DirVolSet., $40
			.db VbrDef.,$02
			.db _C.6
			.db Rest.
			.db VbrDef.,$a1
			.db VbrDef.,$02
			.db VbrDef.,$03
			.db VbrDef.,$04
			.db _Ax5
			.db VbrDef.,$a1
			.db Prefix2., VbrDef.,$02, DirVolSet., $40
			.db VbrDef.,$03
			.db Prefix2.,  PortaSet.,$07,  Dx6,Porta | PortaNote
			.db PortaStep.
			.db PortaStep.
			.db PortaStep.
			.db VibratoStep.
			.db VbrDef.,$02
			.db _Dx5
			.db Rest.
			.db _C.5
			.db Rest.
			.db _F.4
			.db Rest.
			.db _Gx4
			.db Rest.
			.db VbrDef.,$a1
			.db VibratoStep.
			.db Prefix2.,  PortaSet.,$09,  Ax4,Porta | PortaNote
			.db PortaStep.
			.db _Ax4
			.db Rest.
			.db VbrDef.,$a1
			.db VibratoStep.
			.db VibratoStep.
			.db VbrDef.,$02
			.db VibratoStep.
			.db VbrDef.,$03
			.db _D.5
			.db Rest.
			.db Ax4, VolSet. | $40
			.db Rest.
			.db _Dx5
			.db DirVolSet., $40
			.db Prefix2.,  PortaSet.,$03,  F.5,Porta | PortaNote
			.db PortaStep.
			.db PortaStep.
			.db PortaStep.
			.db PortaStep.
			.db PortaStep.
			.db PortaStep.
			.db PortaStep.
			.db Prefix2., FineSlideVolDown.,$01, VbrDef.,$a1
			.db Prefix2., FineSlideVolDown.,$01, VibratoStep.
			.db Prefix2., FineSlideVolDown.,$01, VibratoStep.
			.db Prefix2., FineSlideVolDown.,$01, VbrDef.,$02
			.db Prefix2., FineSlideVolDown.,$01, VibratoStep.
			.db Prefix2., FineSlideVolDown.,$01, VibratoStep.
			.db Prefix2., FineSlideVolDown.,$01, VbrDef.,$03
			.db Prefix2., FineSlideVolDown.,$01, VibratoStep.
			.db Prefix2., FineSlideVolDown.,$01, VbrDef.,$04
			.db Prefix2., FineSlideVolDown.,$01, VibratoStep.
			.db Return.

PBC4:
			.db _F.4
			.db VbrDef.,$a1
			.db VibratoStep.
			.db VbrDef.,$02
			.db Prefix2.,  PortaSet.,$09,  C.5,Porta | PortaNote
			.db PortaStep.
			.db PortaStep.
			.db VbrDef.,$a1
			.db VbrDef.,$02
			.db VbrDef.,$03
			.db Prefix2., WFset.,01, _F.4
			.db Prefix2., WFset.,02, _F.4
			.db _C.5
			.db F.4, VolSet. | $40
			.db _F.5
			.db C.5, VolSet. | $40
			.db _Dx5
			.db Rest.
			.db VbrDef.,$a1
			.db VbrDef.,$02
			.db Prefix2.,  PortaSet.,$09,  C.5,Porta | PortaNote
			.db PortaStep.
			.db PortaStep.
			.db VbrDef.,$a1
			.db VibratoStep.
			.db VbrDef.,$02
			.db Prefix2., WFset.,01, _F.4
			.db Prefix2., WFset.,02, _F.4
			.db _Dx5
			.db F.4, VolSet. | $40
			.db _F.5
			.db Dx5, VolSet. | $40
			.db _D.5
			.db F.5, VolSet. | $40
			.db _D.5
			.db Prefix2.,  PortaSet.,$05,  Ax4,Porta | PortaNote
			.db PortaStep.
			.db PortaStep.
			.db VbrDef.,$a1
			.db VibratoStep.
			.db VbrDef.,$02
			.db VibratoStep.
			.db _D.5
			.db Rest.
			.db Ax4, VolSet. | $40
			.db _C.5
			.db _Ax4
			.db Rest.
			.db Prefix2.,  PortaSet.,$03,  C.5,Porta | PortaNote
			.db PortaStep.
			.db PortaStep.
			.db PortaStep.
			.db VbrDef.,$a1
			.db VibratoStep.
			.db VibratoStep.
			.db VbrDef.,$02
			.db VibratoStep.
			.db VibratoStep.
			.db VbrDef.,$03
			.db VibratoStep.
			.db VibratoStep.
			.db VbrDef.,$04
			.db VibratoStep.
			.db VibratoStep.
			.db Return.
			
PCC0:
			.db	Prefix3., PanVol.,$12, ArpMode.,ArpDisable., Rest.
			.db	RestLoop.,16
			.db C.6, VolSet. | $ff
			.db Prefix3., PortaSet.,$0f,  C.5,Porta | PortaNote,  DirVolSet.,00
			.db PortaStep.
			.db PortaStep.
			.db PortaStep.
			.db Return.


PCC4:
			.db _F.4
			.db VbrDef.,$a1
			.db VibratoStep.
			.db VbrDef.,$02
			.db Prefix2.,  PortaSet.,$09,  C.5,Porta | PortaNote
			.db PortaStep.
			.db PortaStep.
			.db VbrDef.,$a1
			.db VbrDef.,$02
			.db VbrDef.,$03
			.db Prefix2., WFset.,01, _F.4
			.db Prefix2., WFset.,02, _F.4
			.db _C.5
			.db F.4, VolSet. | $40
			.db _F.5
			.db C.5, VolSet. | $40
			.db _Gx5
			.db Rest.
			.db VbrDef.,$a1
			.db VbrDef.,$02
			.db Prefix2.,  PortaSet.,$09,  Ax5,Porta | PortaNote
			.db PortaStep.
			.db PortaStep.
			.db VbrDef.,$a1
			.db VibratoStep.
			.db VbrDef.,$02
			.db Prefix2., WFset.,01, _Dx6
			.db Prefix2., WFset.,02, _Dx6
			.db _C.6
			.db Dx6, VolSet. | $40
			.db _Gx5
			.db Dx5, VolSet. | $40
			.db _Ax5
			.db C.6, VolSet. | $40
			.db _Dx6
			.db Prefix2.,  PortaSet.,$05,  F.6,Porta | PortaNote
			.db PortaStep.
			.db PortaStep.
			.db VbrDef.,$a1
			.db VibratoStep.
			.db VbrDef.,$02
			.db _F.6
			.db Rest.
			.db Dx6, VolSet. | $40
			.db _C.6
			.db _Dx6
			.db Rest.
			.db Prefix2.,  PortaSet.,$09,  C.6,Porta | PortaNote
			.db PortaStep.
			.db PortaStep.
			.db PortaStep.
			.db VbrDef.,$a1
			.db VibratoStep.
			.db VibratoStep.
			.db VbrDef.,$02
			.db VibratoStep.
			.db VibratoStep.
			.db VbrDef.,$03
			.db VibratoStep.
			.db VibratoStep.
			.db VbrDef.,$04
			.db VibratoStep.
			.db VibratoStep.
			.db VibratoStep.
			.db Return.

PEC5:	
			.db Prefix2., ArpMode.,ArpDisable.
			.db _C.5
			.db _Dx5
			.db _F.5
			.db C.5, VolSet. | $40 
			.db Dx5, VolSet. | $40 
			.db F.5, VolSet. | $40 
			.db C.5, VolSet. | $50 
			.db Dx5, VolSet. | $50 
			.db F.5, VolSet. | $50 
			.db C.5, VolSet. | $60 
			.db Dx5, VolSet. | $60 
			.db F.5, VolSet. | $60 
			.db C.5, VolSet. | $80 
			.db Dx5, VolSet. | $80 
			.db F.5, VolSet. | $80
			.db DirVolSet., $ff 
			.db RestLoop., 47
			.db Return.

PEC3:	
			.db _F.1
			.db RestLoop.,30
			.db Prefix2.,  PortaSet.,$2f,  F.1,Porta | PortaDown
			.db PortaStep.
			.db Prefix2., DirVolSet.,$40, PortaStep.
			.db Prefix2., DirVolSet.,$80, PortaStep.
			.db Prefix2., DirVolSet.,$ff, PortaStep.
			.db Prefix2., PortaAutoOff., RestLoop.,26
			.db Return.

DDA1:
			.db RestLoop.,46
				.db Prefix3., DirVolSet.,$00, PanVol.,$00
			.db Sample_E.
			.db Rest.
			.db Rest.
			.db Sample_C.
			.db Sample_C.
			.db Rest.
			.db Rest.
			.db Sample_C.
			.db Rest.
			.db Rest.
			.db Rest.
			.db Sample_C.
			.db Rest.
			.db Sample_E.
			.db Rest.
			.db Sample_E.
			.db Sample_E.
			.db Return.

DDA2:
			.db Sample_C.
			.db Rest.
			.db Sample_D.
			.db Sample_D.
			.db Sample_E.
			.db Rest.
			.db Sample_C.
			.db Rest.
			.db Sample_D.
			.db Sample_D.
			.db Sample_C.
			.db Rest.
			.db Sample_E.
			.db Rest.
			.db Sample_C.
			.db Sample_D.
			.db Sample_C.
			.db Rest.
			.db Sample_D.
			.db Sample_D.
			.db Sample_E.
			.db Rest.
			.db Sample_C.
			.db Rest.
			.db Sample_D.
			.db Sample_D.
			.db Sample_C.
			.db Rest.
			.db Sample_E.
			.db Rest.
			.db Sample_D.
			.db Sample_E.
			.db Sample_C.
DDA4_2			
			.db Rest.
			.db Sample_D.
			.db Sample_D.
			.db Sample_E.
			.db Rest.
			.db Sample_C.
			.db Rest.
			.db Sample_D.
			.db Sample_D.
			.db Sample_C.
			.db Rest.
			.db Sample_E.
			.db Rest.
			.db Sample_C.
			.db Sample_D.
			.db Sample_C.
			.db Rest.
			.db Sample_D.
			.db Sample_D.
			.db Sample_E.
			.db Rest.
			.db Sample_C.
			.db Rest.
			.db Sample_D.
			.db Sample_D.
			.db Sample_C.
			.db Rest.
			.db Sample_E.
			.db Rest.
			.db Sample_E.
			.db Sample_E.
			.db Return.

DDA3:
			.db Prefix2., DirVolSet.,$20, Sample_C.
			.db Rest.
			.db Prefix2., DirVolSet.,$40, Sample_C.
			.db Rest.
			.db Prefix2., DirVolSet.,$60, Sample_C.
			.db Rest.
			.db Prefix2., DirVolSet.,$80, Sample_C.
			.db Rest.
			.db Prefix2., DirVolSet.,$A0, Sample_C.
			.db Prefix2., DirVolSet.,$60, Sample_E.
			.db Prefix2., DirVolSet.,$50, Sample_E.
			.db Prefix2., DirVolSet.,$40, Sample_E.
			.db Rest.
			.db Prefix2., DirVolSet.,$00, Sample_D.
			.db Sample_C.
			.db Rest.
			.db Prefix2., DirVolSet.,$40, Sample_C.
			.db Rest.
			.db Prefix2., DirVolSet.,$60, Sample_D.
			.db Prefix2., DirVolSet.,$00, Sample_D.
			.db Prefix2., DirVolSet.,$40, Sample_D.
			.db Prefix2., DirVolSet.,$00, Sample_E.
			.db Prefix2., DirVolSet.,$00, Sample_C.
			.db Prefix2., DirVolSet.,$60, Sample_D.
			.db Prefix2., DirVolSet.,$00, Sample_E.
			.db Prefix2., DirVolSet.,$40, Sample_D.
			.db Prefix2., DirVolSet.,$00, Sample_E.
			.db Rest.
			.db Sample_E.
			.db Sample_E.
			.db Return.

DDA4_0:
			.db Sample_E.
			.db Sample_E.
			.db Sample_D.
			.db Sample_E.
			.db Sample_C.
			.db Return.

DDA4_1:
			.db Sample_D.
			.db Sample_D.
			.db Sample_C.
			.db Sample_D.
			.db Sample_E.
			.db Sample_D.
			.db Sample_E.
			.db Sample_E.
			.db Return.

DDA7_0:
			.db Rest.
			.db Rest.
			.db Sample_E.
			.db Rest.
			.db Rest.
			.db Rest.
			.db Sample_E.
			.db Rest.
			.db Rest.
			.db Rest.
			.db Return.


DDA8:
			.db Sample_C.
			.db Rest.
			.db Sample_D.
			.db Sample_D.
			.db Sample_D.
			.db Rest.
			.db Sample_D.
			.db Rest.
			.db Sample_D.
			.db Rest.
			.db Sample_D.
			.db Sample_D.
			.db Sample_D.
			.db Rest.
			.db Sample_D.
			.db Rest.
			.db Sample_C.
			.db Rest.
			.db Sample_D.
			.db Sample_D.
			.db Sample_D.
			.db Rest.
			.db Sample_D.
			.db Rest.
			.db Sample_D.
			.db Sample_D.
			.db Sample_D.
			.db Rest.
			.db Sample_D.
			.db Rest.
			.db Sample_D.
			.db Rest.
			.db Sample_C.
			.db Rest.
			.db Sample_D.
			.db Sample_D.
			.db Sample_D.
			.db Rest.
			.db Sample_D.
			.db Rest.
			.db Sample_D.
			.db Rest.
			.db Sample_D.
			.db Sample_D.
			.db Sample_D.
			.db Rest.
			.db Sample_D.
			.db Rest.
			.db Sample_C.
			.db Rest.
			.db Sample_D.
			.db Sample_D.
			.db Sample_D.
			.db Rest.
			.db Sample_D.
			.db Rest.
			.db Sample_D.
			.db Sample_D.
			.db Sample_D.
			.db Rest.
			.db Sample_D.
			.db Rest.
			.db Sample_D.
			.db Rest.
			.db Return.

DDAC_0:
			.db Rest.
			.db Rest.
			.db Sample_E.
			.db Rest.
			.db Rest.
			.db Rest.
			.db Rest.
			.db Rest.
			.db Rest.
			.db Rest.
			.db Sample_E.
			.db Rest.
			.db Sample_E.
			.db Sample_E.
			.db Return.
				
;notes
C.1=$01     
Cx1=$02
D.1=$03
Dx1=$04
E.1=$05
F.1=$06
Fx1=$07    
G.1=$08
Gx1=$09    
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

;//Functions

Rest. = $00
VbrDef. = $8f
EnvDef0. = $90
EnvDef1. = $91
EnvDef2. = $92
KeySet. = $93
KeyRls.	= $94
NoteCut. = $95
Call. = $96
Return. = $97
Jump. = $98
BaseVol. = $99
PanVol. = $9a
WFset. = $9b
TickSet. = $9c
Prefix. = $9d
MasterPan. = $9e
FineSlideVolUp. = $9f
FineSlideVolDown. = $a0
DirVolSet. = $a1
DirVolMode. = $a2
EnvMode. = $a3
EnvSelect. = $a4
RestLoop. = $a5
MasterVolFade. = $a6
PortaSet. = $a7
PortaOn. = $a8
PortaPause. = $a9
PortaStep. = $aa
PortaAutoOn. = $ab
PortaAutoOff. = $ac
ArpSet.	= $ad
ArpMode. = $ae
Combo1. = $af
ArpStep. = $b1
CallLen. = $b2
KeyDisable. = $b3
DDA. = $b4
Arp_Key. = $b5
PortaStepDown. = $b6
Prefix2. = $b7
Prefix3. = $b8
Prefix4. = $b9
FineSlideVolDown1. = $ba
VibratoStep. = $bb



;//Arguments
Porta = $0e
PortaOff = $00
PortaUp = $10
PortaDown = $20
PortaNote = $30
ArpStepC1. = $01
KeyRlsC1.	= $02
VolSlideUp. = $07
VolSlideDown. = $08
VolSet. = $03
ArpNoteTrig. = $01
StepArp. = $0b
;Arpeggio. = $10
;Portamento. = $02
;Vibrato. = $40
ArpDisable. = $00
PortaDown. = $10

Sample_C. = $00+$bc
Sample_D. = $01+$bc
Sample_E. = $02+$bc

C. = $00
D. = $01
E. = $02


;//##################################################################################################
;Samples

	
Wavefiles:

.TBL.lo
	.db low(.kick),low(.hihat),low(.snare)
.TBL.hi
	.db high(.kick),high(.hihat),high(.snare)
.TBL.bnk
	.db bank(.kick),bank(.hihat),bank(.snare)

.kick
	incbin "t_0C.wav.wf"
.hihat
	incbin "t_0D.wav.wf"
.snare	
	incbin "t_0E.wav.wf"
.endsample
	
SampleTBL.lo = .TBL.lo
SampleTBL.hi = .TBL.hi
SampleTBL.bnk = .TBL.bnk


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
	.db $10,$11,$12,$13,$14,$15,$16,$17  ;
	.db $18,$19,$1a,$1b,$1c,$1d,$1f,$1e  ;	Saw (ramp up)
	.db $00,$01,$02,$03,$04,$05,$06,$07  ;
	.db $08,$09,$0a,$0b,$0c,$0d,$0e,$0f  ;
																			 ;
	.db $06,$06,$07,$08,$09,$0b,$0d,$13  ;	Sine
	.db $11,$12,$13,$14,$14,$15,$16,$16  ;
	.db $17,$17,$18,$18,$17,$16,$15,$13  ;
	.db $11,$0f,$0d,$0b,$09,$08,$07,$06  ;
;.......................................

TestSongEnd:

SongSize = TestSongEnd - TestSong

;	.code
;	.bank $1f
	