
;...............................................................................
;//Print string ascii string
;///////////////////////////
; vram addr in X:A
; string addr in R0
;
PrintString:
    bcs .skip
	    stz <vdc_reg
	    st0 #$00
	    sta $0002
	    stx $0003
.skip
	    lda #$02
	    sta <vdc_reg
	    st0 #$02
	    cly

.ll01
	    lda [R0],y
	  beq .out
	    sec
	    sbc #$20
	    sta $0002
	    st2 #$01
	    iny
    bra .ll01
.out
  rts
  
  
  
  
  
;...............................................................................
;//Print variable to x,y location
;/ X:A vram addr
;/ y=value
;/ Carry; 1=append last location, 0=location in X:A
PrintByte:
    bcs .skip
	    stz <vdc_reg
	    st0 #$00
	    sta $0002
	    stx $0003
.skip   
	    lda #$02
	    sta <vdc_reg
	    st0 #$02
	    tya
	    tax
	    lsr a
	    lsr a
	    lsr a
	    lsr a
	    tay
	    lda hex_conv,y
	    sec
	    sbc #$20
	    sta $0002
	    st2 #$01
	    txa
	    and #$0f
	    tay
	    lda hex_conv,y
	    sec
	    sbc #$20
	    sta $0002
	    st2 #$01
  rts 

print_lo_nibble:
    bcs .skip
	    stz <vdc_reg
	    st0 #$00
	    sta $0002
	    stx $0003
	    lda #$02
	    sta <vdc_reg
	    st0 #$02
.skip   
	    tay
	    and #$0f
	    tay
	    lda hex_conv,y
	    sec
	    sbc #$20
	    sta $0002
	    st2 #$01
  rts 

print_hi_nibble:
    bcs .skip
	    stz <vdc_reg
	    st0 #$00
	    sta $0002
	    stx $0003
	    lda #$02
	    sta <vdc_reg
	    st0 #$02
.skip   
	    tay
	    lsr a
	    lsr a
	    lsr a
	    lsr a
	    tay
	    lda hex_conv,y
	    sec
	    sbc #$20
	    sta $0002
	    st2 #$01
  rts 

print_indent:
.ll01
	    st1 #$00
	    st2 #$00
	    dey
    bne .ll01
  rts
    
;near data table
hex_conv:
    .db '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'


;...............................................................................
;//Clears the screen with blank space
;/ Note: no arguments, no whining. 
;/ Note: This the 64x32 version.
ClearScreen:
			st0 #$00
			st1 #$00
			st2 #$00
			st0 #$02
			lda #$02
			sta <vdc_reg
			clx
			ldy #$08
.loop
			st1 #$00
			st2 #$01
			inx 
		bne .loop
			dey
		bne .loop
	rts
	
;...............................................................................
;//Inintialize self modifying code Txx
;
;
init_dma:
			tii .start_code,__DMA,(.end_code-.start_code)
	rts

.start_code
			tia $0f0f,$0f0f,$0f0f
	rts
.end_code	

;...............................................................................
;//Inintialize self modifying code Txx
;
__wait_vblank:
			lda #$01
			sta __vblank
.loop
			lda __vblank
			bne .loop
			dex
			bne __wait_vblank
	rts
			