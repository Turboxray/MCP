;Initialise hardware

;#sub
init_vdc:
	
	VDC_REG MAWR , $0000
	VDC_REG MARR , $0000
	VDC_REG CR , $0000
	VDC_REG RCR , $0000
	VDC_REG BXR , $0000
	VDC_REG BYR , $0000
	VDC_REG MWR , $0010
	VDC_REG VSR , $0F02
	VDC_REG VDR , $00EF
	VDC_REG VDE , $0003	
	VDC_REG DCR , $0000
	VDC_REG HSR , $0202
	VDC_REG HDR , $041f
	
	clx
	ldy #$80
	st0 #$00
	st1 #$00
	st2 #$00
	st0 #$02
.loop
	stz $0002
	stz $0003
	inx
	bne .loop
	iny
	bne .loop		
	
	rts

;#end sub



;#sub
init_system:
	
			;init interrupts
	stz     $0C00   	
        lda     #$07    
        sta     $1402   	
        stz     $1403   	;noted from doc(?) - acknowledge int
	
	jmp main

;#end sub



;#sub
init_set_res:
	
		lda #L_RES		;setup horizontal res
		sta $400
	
	rts

;#end sub



;#sub
init_wsg:

		ldx #$05
.loop
		stx $800
		stz $801
		stz $802
		stz $803
		stz $804
		stz $805
		stz $806
		stz $807
		stz $808
		stz $809
		stz $80a
		stz $80b
		stz $80c
		stz $80d
		stz $80e
		stz $80f
		dex
		bpl .loop
		
		stz $801
	rts
;#end sub





