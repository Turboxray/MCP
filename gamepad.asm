;###############################################
; 
; JOYSTICK port
;
; required variables:
;	
;		b1_sts
;		b2_sts
;		sl_sts
;		st_sts
;		up_sts
;		dn_sts
;		lf_sts
;		rh_sts
;
; value returned is : TRUE/FALSE 
;
;

  

READ_IO:
;
; new and improved
;
      pha
			phx
      lda #$01
      sta $1000
      lda #$03
      sta $1000
      lda #$01
      sta $1000
      pha
      pla
      nop
      nop
      lda $1000
      eor #$0f
      tax
      and #$01
      sta up_sts
      txa
			and #$04
			sta dn_sts
			txa
			and #$08
			sta lf_sts
			txa
			and #$02
			sta rh_sts
      
      
      stz $1000
      pha
      pla
      nop
      nop
      lda $1000
      eor #$0f
      tax
      and #$01
      sta b1_sts
      txa
			and #$02
			sta b2_sts
			txa
			and #$04
			sta sl_sts
			txa
			and #$08
			sta st_sts
 
 			;first check for reset status
 			lda up_sts
 			ora dn_sts
 			ora lf_sts
 			ora rh_sts
 			ora b1_sts
 			ora b2_sts
 			bne .exit
 			
 			;now check to see if reset enabled
			lda sl_sts
			ora st_sts
			cmp #$0c
			bne .exit
			lda soft_reset
			cmp #$4E					;N
			bne .exit
			lda soft_reset+1
			cmp #$45					;E
			bne .exit
			lda soft_reset+2
			cmp #$43					;C
			bne .exit
			jmp [$FFFE]
			
.exit
			plx
      pla    
    rts 
;#end

;//Variable defines
	.bss
		up_sts:				.ds 1
		dn_sts:				.ds 1
		lf_sts:				.ds 1
		rh_sts:				.ds 1
		sl_sts:				.ds 1
		st_sts:				.ds 1
		b1_sts:				.ds 1
		b2_sts:				.ds 1
		soft_reset:		.ds 3
;end

	.code
