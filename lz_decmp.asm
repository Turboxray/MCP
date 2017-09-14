

;// Pucrunch decompression routine - standalone ver.
;//
;// Modified for use with PCE(CD) system.
;// rev. 1.3
;//
;// Calling parameters for LZ_decmp:
;// (stream) source address = _bx
;// (stream) source bank = _al
;// (mode)   _ah = 0, 1, 2, 3
;//
;//						0 = decompress directly to VRAM (no conversion)
;//						1 = decompress directly to VRAM (convert linear to planar, per tile)
;//						2 = decompress directly to VRAM (convert linear to planar, per sprite)
;//						3 = decompress directly to CRAM
;//
;//
;//
;// Note:	The destination address of the PORT devices, needs to be set first before calling
;//				the decompression routine.
;//
;// Note:	Function destroys _al,_ah,_bl,_bh,_cl
;//
;// 
;// Note: routine uses self modifying code.
;// 
;// Header: 
;// end compressed addr(word)
;// indentifier 'pu' (word)
;// ending decompressed addr(word) 
;// escape char (byte)
;// starting decompressed addr (word) 
;// gamma info (4 bytes)
;// exec addr (word)
;// 
;// 
;//	Todo:		
;//						-Need to include a RAM/Local decompression mode (non port based).
;//						-Need to include a ADPCM port write decompression mode.
;//						-Need to include a ADPCM port read decompression mode.  
;//
;//
;// History:
;//
;//	9/19/10- Removed support for HuC. PCEAS only.
;//					 Fixed possible bug for calling VRAM decompression after a VCE decompression.
;//
;// 5/23/08- Adapted for HuC.
;//          Changed to ring buffer system (uses 1 8k ram bank)
;//
;// 7/28/07- Added automatic source bank incremet code.
;//          It uses one 8k segment.
;//
;// 7/28/07- Initial release.
;//
;// 
;//
;//

	.zp
	LZ_SZE:	.ds 2
	LZ_AND:	.ds 2
	LZ_EOR:	.ds 2
	
	.code


LZ_decmp_vce:
    lda #$ea
    sta __port_mode
    sta __port_mode+1
    sta __port_mode+2
    lda #$05								;setup IO port mode for VCE
    sta __IO_port
    lda #$04
    sta __IO_port+1
    bra pu_decmp
    
    
LZ_decmp_vdc:
    lda #$ea
    sta __port_mode
    sta __port_mode+1
    sta __port_mode+2
    bra pu_decmp

LZ_decmp:
	  lda #$03								;I think this needs to be re/initialized on every call just to be safe.
	  sta __IO_port
	  stz __IO_port+1
    stz decmp_cntr
    lda <_ah
    cmp #$01
    bcc LZ_decmp_vdc
    beq .tile
    cmp #$03
    bcc .sprite
.vce_mode
		jmp LZ_decmp_vce
    rts                     
    												;These two modes decode linear to planar, before writing to vram
.sprite
    lda #$4c
    sta __port_mode
    lda #LOW(lin2plnr)
    sta __port_mode+1
    lda #HIGH(lin2plnr)
    sta __port_mode+2
    bra pu_decmp
.tile   
    lda #$4c
    sta __port_mode
    lda #LOW(lin2cmplnr)
    sta __port_mode+1
    lda #HIGH(lin2cmplnr)
    sta __port_mode+2
    bra pu_decmp
    
    


  ;processor Huc6280 - duh!

                  ;// zeropage usage
LZPOS 	EQU _al   ; 2 ZeroPage temporaries
bitstr  EQU _cl   ; 1 temporary (does not need to be ZP)

                  ; Carry will be set for error, cleared for OK.
  

  pu_decmp:

    sei
    tma #$03
    pha
    tma #$04
    pha
    lda #$7f
    tam #$03
    jsr decompress
    pla
    tam #$04
    pla 
    tam #$03
    rts
    
    

decompress:        

                      ; Setup read pointer
    ldy <_bx
    lda <_bx+1
    and #$1f
    ora #$80
    tax
    sty INPOS
    stx INPOS+1

                      ;// setup source bank and range (single 8k segment)
   
    lda <_al
_SETUP_MPR:   
    tam #$04
    
    
                      ;// increment 2bytes past start of header
    jsr getnew
    jsr getnew
    
    jsr getbyt        ; 'p'
    cmp #112
    beq .9
    sec   ; error
    jmp eof2
.9  
    jsr getbyt        ; 'u'
    cmp #117
    beq .8
    sec   ; error
    jmp eof2
.8
    jsr getbyt        ; skip endAddr
    jsr getbyt
    jsr getbyt

    sta esc+1         ; starting escape

    jsr getbyt        ; read startAddr
    lda #$00          ;// changed startAddr to ZP parameter
    sta OUTPOS
    jsr getbyt
    lda #$60
    sta OUTPOS+1
    jsr getbyt        ; read # of escape bits
    sta escB0+1
    sta escB1+1
    lda #8
    sec
    sbc escB1+1
    sta noesc+1       ; 8-escBits

    jsr getbyt
    sta mg+1          ; maxGamma + 1
    lda #9
    sec
    sbc mg+1          ; 8 - maxGamma == (8 + 1) - (maxGamma + 1)
    sta longrle+1
    jsr getbyt
    sta mg1+1         ; (1<<maxGamma)
    asl a
    clc
    sbc #0
    sta mg21+1        ; (2<<maxGamma) - 1
    jsr getbyt
    sta elzpb+1

    jsr getbyt        ; exec address
    ;sta lo+1         ; lo
    jsr getbyt
    ;sta hi+1         ; hi

    jsr getbyt        ; rleUsed
    ldx #0
    tay
.0  
    beq .1            ; Y == 0 ?
    jsr getbyt
    sta table,x       ;// this is the destination address.
    inx
    dey
    bne .0
.1                    ; setup bit store - $80 means empty
    lda #$80
    sta <bitstr
    jmp pu_main

  getbyt:
    jsr getnew
    lda <bitstr
    ror a
    rts


  newesc: 
    ldy esc+1         ; remember the old code (top bits for escaped byte)
  escB0:  
    ldx #2            ; ** PARAMETER  0..8
    jsr getchkf       ; get & save the new escape code
    sta esc+1
    tya               ; pre-set the bits
  
                      ; Fall through and get the rest of the bits.
  noesc:
    ldx #6            ; ** PARAMETER  8..0
    jsr getchkf
    jsr putch         ; output the escaped/normal byte

                      ; Fall through and check the escape bits again
  pu_main:
    ldy #0            ; Reset to a defined state
    tya               ; A = 0
  escB1:
    ldx #2            ; ** PARAMETER  0..8
    jsr getchkf       ; X = 0
  esc:
    cmp #0
    bne noesc
    
                      ; Fall through to packed code

    jsr getval        ; X = 0
    sta <LZPOS        ; xstore - save the length for a later time
    lsr a             ; cmp #1  ; LEN == 2 ? (A is never 0)
    bne lz77          ; LEN != 2  -> LZ77
    ;tya              ; A = 0
    jsr get1bit       ; X = 0
    lsr a             ; bit -> C, A = 0
    bcc lz77_2        ; A=0 -> LZPOS+1

                      ;***FALL THRU***

                      ; e..e01
    jsr get1bit       ; X = 0
    lsr a             ; bit -> C, A = 0
    bcc newesc        ; e..e010
                      ;***FALL THRU***

                      ; e..e011
  srle:
    iny               ; Y is 1 bigger than MSB loops
    jsr getval        ; Y is 1, get len, X = 0
    sta <LZPOS        ; xstore - Save length LSB
  mg1:
    cmp #64           ; ** PARAMETER 63-64 -> C clear, 64-64 -> C set..
    bcc chrcode       ; short RLE, get bytecode

  longrle:
    ldx #2            ; ** PARAMETER  111111xxxxxx
    jsr getbits       ; get 3/2/1 more bits to get a full byte, X = 0
    sta <LZPOS        ; xstore - Save length LSB

    jsr getval        ; length MSB, X = 0
    tay               ; Y is 1 bigger than MSB loops

  chrcode:
    jsr getval        ; Byte Code, X = 0
    tax               ; this is executed most of the time anyway
    lda table-1,x     ; Saves one jump if done here (loses one txa)

    cpx #32           ; 31-32 -> C clear, 32-32 -> C set..
    bcc .1            ; 1..31, we got the right byte from the table

                      ; Ranks 32..64 (11111°xxxxx), get byte..
    txa               ; get back the value (5 valid bits)
    ldx #3
    jsr getbits       ; get 3 more bits to get a full byte, X = 0

.1  
    ldx <LZPOS        ; xstore - get length LSB
    inx               ; adjust for cpx#$ff;bne -> bne
  dorle:
    jsr putch
    dex
    bne dorle         ; xstore 0..255 -> 1..256
    dey
    bne dorle         ; Y was 1 bigger than wanted originally
  mainbeq:
    beq pu_main       ; reverse condition -> jump always


  lz77:
    jsr getval        ; X = 0
  mg21:
    cmp #127          ; ** PARAMETER  Clears carry (is maximum value)
    bne noeof

                      ; EOF
  eof:
                      ; EOF
  eof:  
    clc
  eof2:
    ;**lda #$37
    ;**sta <1
    cli
  hi:
    ldx #0
  lo:
    ldy #0
    rts


  noeof:
    sbc #0            ; C is clear -> subtract 1  (1..126 -> 0..125)
  elzpb:
    ldx #0            ; ** PARAMETER (more bits to get)
    jsr getchkf       ; clears Carry, X = 0

  lz77_2:
    sta <LZPOS+1      ; offset MSB
    jsr getbyte       ; clears Carry, X = 0
                      ; Note: Already eor:ed in the compressor..
    ;eor #255         ; offset LSB 2's complement -1 (i.e. -X = ~X+1)
    adc OUTPOS        ; -offset -1 + curpos (C is clear)
    ldx <LZPOS        ; xstore = LZLEN (read before it's overwritten)
    sta <LZPOS

    lda OUTPOS+1
    sbc <LZPOS+1      ; takes C into account
    and #$1f          ; //# new code
    clc
.sm1
    eor #$60
    sta <LZPOS+1      ; copy X+1 number of chars from LZPOS to OUTPOS
    ;ldy #0           ; Y was 0 originally, we don't change it

LZ_OVR0 = .sm1+1

    inx               ; adjust for cpx#$ff;bne -> bne
  lzloop:
    lda [LZPOS]       ; using abs,y is 3 bytes longer, only 1 cycle/byte faster
    jsr putch         ; Note: must be copied forwards!
    inc <LZPOS        ; //# new code
    bne .skip
    lda LZPOS+1
    inc a
    sec
    sbc <LZ_SZE
    and <LZ_AND
    eor <LZ_EOR
.sm3
    sta LZPOS+1
.skip
    iny               ; Y does not wrap because X=0..255 and Y initially 0
    dex
    bne lzloop        ; X loops, (256,1..255)
    beq mainbeq       ; jump through another beq (-1 byte, +3 cycles)

LZ_OVR1 = .sm3+1

  getnew:
    pha               ; 1 Byte/3 cycles


INPOS1: 
;BNK_OVRFLW = INPOS1+15
;SRC_LO = INPOS1+24
;SRC_HI = INPOS1+28
INPOS = INPOS1+1

    lda $aaaa         ; ** PARAMETER
    pha
    inc INPOS
    bne .0
    lda INPOS+1
    inc a
    sta INPOS+1
    cmp #$A0
    bcc .0
    and #$1f
    ora #$80
    sta INPOS+1
    tma #$04
    inc a
    tam #$04

.0
    pla
    sec
    rol a             ; Shift out the next bit and
                      ;  shift in C=1 (last bit marker)
    sta <bitstr       ; bitstr initial value = $80 == empty
    pla               ; 1 Byte/4 cycles
    rts
                      ; 25+12 = 37

                      ; getval : Gets a 'static huffman coded' value
                      ; ** Scratches X, returns the value in A **
  getval:
    inx               ; X <- 1
    txa               ; set the top bit (value is 1..255)
  gv0:
    asl <bitstr
    bne .1
    jsr getnew
.1
    bcc getchk        ; got 0-bit
    inx
  mg:
    cpx #7            ; ** PARAMETER unary code maximum length + 1
    bne gv0
    beq getchk        ; inverse condition -> jump always
                      ; getval: 18 bytes
                      ; 15 + 17*n + 6+15*n+12 + 36*n/8 = 33 + 32*n + 36*n/8 cycles

                      ; getbits: Gets X bits from the stream
                      ; ** Scratches X, returns the value in A **
  getbyte:
    ldx #7
  get1bit:
    inx               ;2
  getbits:
    asl <bitstr
    bne .1
    jsr getnew
.1
    rol a             ;2
  getchk:
    dex               ;2    more bits to get ?
  getchkf:
    bne getbits       ;2/3
    clc               ;2    return carry cleared
    rts               ;6+6



OUTPOS1:
OUTPOS = OUTPOS1+1    ; ZP
putch:
    sta $bbbb         ; ** parameter
    
    nop								; jmp pixel_cnvrt
    nop
    nop 

.ll pha               ;save A in case of RLE
    lda .sm+1
    eor #$01
    sta .sm+1
    pla
.sm sta $0002
  
.buffer
    inc OUTPOS    
    bne .0
    lda OUTPOS+1      ; //# new code
    inc a
    and #$1f    
    clc
.sm2
    eor #$60
    sta OUTPOS+1
.0
    rts

__port_mode = putch+3
__IO_port = .sm+1
LZ_OVR2 = .sm2+1
bra_1 = .sm - .ll
bra_2 = .buffer - .ll
decmp_return = .buffer


endofcode:



  
  .org endofcode
  

table:  .db 0,0,0,0,0,0,0,0
        .db 0,0,0,0,0,0,0,0
        .db 0,0,0,0,0,0,0,0
        .db 0,0,0,0,0,0,0,0



branch_code: .db bra_1,bra_2
              
decmp_cntr: .db $00
        
work_buffer.ASM:        ;for sprites or tiles ;)

        .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0        
        .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0        
        .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0        
        .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0        
        .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0        
        .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0        
        .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0        
        .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0   
        
  
lin2cmplnr:
    lda decmp_cntr
    inc a
    and #$1f
    sta decmp_cntr
    beq .skip
    lda INPOS
    sec
    sbc #$20
    sta .get_pixel+1
    lda INPOS+1
    sbc #$00
    and #$1f
    ora #$80
    sta .get_pixel+2

    phx
    phy

    clx
.get_pixel
    lda $1234,y
    asl a
    rol work_buffer.ASM+0,x
    asl a
    rol work_buffer.ASM+1,x
    asl a
    rol work_buffer.ASM+16,x
    asl a
    rol work_buffer.ASM+17,x
    asl a
    rol work_buffer.ASM+0,x
    asl a
    rol work_buffer.ASM+1,x
    asl a
    rol work_buffer.ASM+16,x
    asl a
    rol work_buffer.ASM+17,x
    iny
    cpy #$08
    bne .get_pixel
    cly
    inx
    cpx #$20
    bcc .get_pixel
    
    tia work_buffer.ASM,$0002,$20

    ply
    plx
.skip
    jmp decmp_return

  
  
lin2plnr:
    phy
    lda decmp_cntr
    inc a
    and #$7f
    sta decmp_cntr
    beq .skip
    lda INPOS
    sec
    sbc #$80
    sta .get_pixel+1
    lda INPOS+1
    sbc #$00
    and #$1f
    ora #$80
    sta .get_pixel+2

    clx
.get_pixel
    lda $1234,y
    asl a
    rol work_buffer.ASM+0,x
    asl a
    rol work_buffer.ASM+32,x
    asl a
    rol work_buffer.ASM+64,x
    asl a
    rol work_buffer.ASM+96,x
    asl a
    rol work_buffer.ASM+0,x
    asl a
    rol work_buffer.ASM+32,x
    asl a
    rol work_buffer.ASM+64,x
    asl a
    rol work_buffer.ASM+96,x
    iny
    cpy #$16
    bne .get_pixel
    cly
    inx
    cpx #$80
    bcc .get_pixel
    
    tia work_buffer.ASM,$0002,$80

.skip   
    ply
    plx
    jmp decmp_return            
            

