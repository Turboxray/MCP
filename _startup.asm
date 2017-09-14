;boot code
	
	;.BANK START_BANK
	.org	$E020
	.code	

_startup:
	SEI			; disable int. - set hi_speed - clear decimal flag
	nop
	csh
	nop
	cld
	
	lda #$ff		;setup I/o & ram banks
	tam #$00
	lda #$f8
	tam #$01	
	ldx #$ff		;setup stack
	txs
	
	stz <$00		;init ram to $00
	tii $2000,$2001,$1fff

	jmp init_system	


