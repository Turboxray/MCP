;====================================================================
;
; PC Engine CD-ROM SYSTEM BIOS  version 3.00
;
; BIOS ENTRY and WORK equ
;
; 910304  Takaki Kobayashi  ..and more ;)
;
;====================================================================
;
max_mapping equ $FFF5

;--------------------------------------------------------------------
; BIOS ENTRY
;
cd_boot     equ $E000
cd_reset    equ $E003
cd_base     equ $E006
cd_read     equ $E009
cd_seek     equ $E00C
cd_exec     equ $E00F
cd_play     equ $E012
cd_search   equ $E015
cd_pause    equ $E018
cd_stat     equ $E01B
cd_subq     equ $E01E
cd_dinfo    equ $E021
cd_contnts  equ $E024
cd_subrd    equ $E027
cd_pcmrd    equ $E02A
cd_fade     equ $E02D

ad_reset    equ $E030
ad_trans    equ $E033
ad_read     equ $E036
ad_write    equ $E039
ad_play     equ $E03C
ad_cplay    equ $E03F
ad_stop     equ $E042
ad_stat     equ $E045

bm_format   equ $E048
bm_free     equ $E04B
bm_read     equ $E04E
bm_write    equ $E051
bm_delete   equ $E054
bm_files    equ $E057

ex_getver   equ $E05A
ex_setvec   equ $E05D
ex_getfnt   equ $E060
ex_joysns   equ $E063
ex_joyrep   equ $E066
ex_scrsiz   equ $E069
ex_dotmod   equ $E06C
ex_scrmod   equ $E06F
ex_imode    equ $E072
ex_vmode    equ $E075
ex_hmode    equ $E078
ex_vsync    equ $E07B
ex_rcron    equ $E07E
ex_rcroff   equ $E081
ex_irqon    equ $E084
ex_irqoff   equ $E087
ex_bgon     equ $E08A
ex_bgoff    equ $E08D
ex_spron    equ $E090
ex_sproff   equ $E093
ex_dspon    equ $E096
ex_dspoff   equ $E099
ex_dmamod   equ $E09C
ex_sprdma   equ $E09F
ex_satclr   equ $E0A2
ex_sprput   equ $E0A5
ex_setrcr   equ $E0A8
ex_setred   equ $E0AB
ex_setwrt   equ $E0AE
ex_setdma   equ $E0B1
ex_binbcd   equ $E0B4
ex_bcdbin   equ $E0B7
ex_rnd      equ $E0BA
ex_colorcmd equ $E0E4

ma_mul8u    equ $E0BD
ma_mul8s    equ $E0C0
ma_mul16u   equ $E0C3
ma_div16s   equ $E0C6
ma_div16u   equ $E0C9
ma_sqrt     equ $E0CC
ma_sin      equ $E0CF
ma_cos      equ $E0D2
ma_atni     equ $E0D5

psg_bios    equ $E0D8
grp_bios    equ $E0DB

ex_memopen  equ $E0DE   ;ver 3.00 or later

psg_drive   equ $E0E1

;--------------------------------------------------------------------
; ZERO PAGE WORK
;
zpgtop      equ $20DC
;
zpg_grp_top equ $20DC
vi_bitpat   equ $20DC   ;1
vi_rvbitpat equ $20DD   ;1
vi_ft_front equ $20DE
vi_padrs    equ $20DE   ;2
vi_porg     equ $20E0   ;1
vi_ft_back  equ $20E1   ;3
vi_stack    equ $20E4   ;2
;
zpg_psg_top equ $20E6
time_sw     equ $20E6   ;1
main_sw     equ $20E7   ;1
si          equ $20E8
si_l        equ $20E8   ;1
si_h        equ $20E9   ;1
r0          equ $20EA
r0_l        equ $20EA   ;1
r0_h        equ $20EB   ;1
;
zpg_sys_top equ $20EC
zx0         equ $20EC
zl0         equ $20EC   ;1
zh0         equ $20ED   ;1
zx1         equ $20EE
zl1         equ $20EE   ;1
zh1         equ $20EF   ;1
zx2         equ $20F0
zl2         equ $20F0   ;1
zh2         equ $20F1   ;1
cdi_b       equ $20F2   ;1
crl_m       equ $20F3   ;1
crh_m       equ $20F4   ;1
irq_m       equ $20F5   ;1
str_b       equ $20F6   ;1
reg_box     equ $20F7   ;1 Note: this is the vdc reg reserve/restore variable.
_ax         equ $20F8
_al         equ $20F8   ;1
_ah         equ $20F9   ;1
_bx         equ $20FA
_bl         equ $20FA   ;1
_bh         equ $20FB   ;1
_cx         equ $20FC
_cl         equ $20FC   ;1
_ch         equ $20FD   ;1
_dx         equ $20FE
_dl         equ $20FE   ;1
_dh         equ $20FF   ;1

;--------------------------------------------------------------------
; RAM WORK
;
ramtop      equ $2200
;
usrvec      equ $2200
irq2_jmp    equ $2200   ;2
irq_jmp     equ $2202   ;2
tim_jmp     equ $2204   ;2
nmi_jmp     equ $2206   ;2
sync_jmp    equ $2208   ;2
rcr_jmp     equ $220A   ;2
bgx1        equ $220C   ;2
bgx2        equ $220E   ;2
bgy1        equ $2210   ;2
bgy2        equ $2212   ;2
sat_adr     equ $2214   ;2
sprptr      equ $2216   ;1
spryl       equ $2217   ;1
spryh       equ $2218   ;1
sprxl       equ $2219   ;1
sprxh       equ $221A   ;1
sprnl       equ $221B   ;1
sprnh       equ $221C   ;1
spral       equ $221D   ;1
sprah       equ $221E   ;1
color_cmd   equ $221F   ;1
bgc_ptr     equ $2220   ;2
bgc_len     equ $2222   ;1
sprc_ptr    equ $2223   ;2
sprc_len    equ $2225   ;1
joykeyflg   equ $2226   ;1
joyena      equ $2227   ;1
joy         equ $2228   ;5
joytrg      equ $222D   ;5
joyold      equ $2232   ;5
irq_cnt     equ $2241   ;1
mwr_m       equ $2242   ;1
dcr_m       equ $2243   ;1
notrdyflg   equ $2247   ;1
rndseed     equ $2249   ;1
rndl        equ $2249   ;1
rndh        equ $224A   ;1
rndm        equ $224B   ;1
tnomin      equ $226A   ;1
tnomax      equ $226B   ;1
outmin      equ $226C   ;1
outsec      equ $226D   ;1
outfrm      equ $226E   ;1
vdtin_flg   equ $2272   ;1
recbase0_h  equ $2274   ;1
recbase0_m  equ $2275   ;1
recbase0_l  equ $2276   ;1
recbase1_h  equ $2277   ;1
recbase1_m  equ $2278   ;1
recbase1_l  equ $2279   ;1
scsists     equ $227B   ;1
suberrc     equ $227C   ;1
subcode     equ $227E   ;1

ramend      equ $22D0

psg_work_top    equ $22D0
graph_work_top  equ $2616
key_work_top    equ $2646
user_work_top   equ $267C

;--------------------------------------------------------------------
; PSG_BIOS FUNCTION NUMBER
;
PSG_ON      equ 0
PSG_OFF     equ 1
PSG_INIT    equ 2
PSG_BANK    equ 3
PSG_TRACK   equ 4
PSG_WAVE    equ 5
PSG_ENV     equ 6
PSG_FM      equ 7
PSG_PE      equ 8
PSG_PC      equ 9
PSG_TEMPO   equ 10
PSG_PLAY    equ 11
PSG_MSTAT   equ 12
PSG_SSTAT   equ 13
PSG_MSTOP   equ 14
PSG_SSTOP   equ 15
PSG_ASTOP   equ 16
PSG_MVOFF   equ 17
PSG_CONT    equ 18
PSG_FDOUT   equ 19
PSG_DCNT    equ 20

;--------------------------------------------------------------------
; GRP_BIOS FUNCTION NUMBER
;
VI_GINIT    equ 0
VI_CASHCLR  equ 1
VI_STRTADR  equ 2
VI_GETADRS  equ 3
VI_CLS      equ 4
VI_PSET     equ 5
VI_POINT    equ 6
VI_LINE     equ 7
VI_BOX      equ 8
VI_BOXF     equ 9
VI_FLOOD    equ 10
VI_PAINT    equ 11
VI_GWINDOW  equ 12
VI_GFONT    equ 13
VI_PUTFONT  equ 14
VI_SYMBOL   equ 15
;--------------------------------------------------------------------



;--------------------------------------------------------------------
vdc_crl = $20F3 ; VDC control register   (copy of)
vdc_crh = $20F4 ;
irq_m   = $20F5 ; interrupt control mask (copy of)
vdc_sr  = $20F6 ; VDC status register    (copy of)
vdc_reg = $20F7 ; VDC register index     (copy of)


;--------------------------------------------------------------------
; This block defines standard parameter-passing
; areas (in zero-page) for subroutines.
;

_bp = $20EC ; base pointer
_si = $20EE ; source address
_di = $20F0 ; destination address
_ax = $20F8
_al = $20F8
_ah = $20F9
_bx = $20FA
_bl = $20FA
_bh = $20FB
_cx = $20FC
_cl = $20FC
_ch = $20FD
_dx = $20FE
_dl = $20FE
_dh = $20FF


;--------------------------------------------------------------------
; VDC (Video Display Controller)

videoport        equ $0000

video_reg       .equ  videoport
video_reg_l      equ  video_reg
video_reg_h      equ  video_reg+1

video_data       equ  videoport+2
video_data_l     equ  video_data
video_data_h     equ  video_data+1


;--------------------------------------------------------------------
; SGX VDC (Video Display Controller)

sgx_videoport     equ $0010

sgx_video_reg     equ  sgx_videoport
sgx_video_reg_l   equ  sgx_video_reg
sgx_video_reg_h   equ  sgx_video_reg+1

sgx_video_data    equ  sgx_videoport+2
sgx_video_data_l  equ  sgx_video_data
sgx_video_data_h  equ  sgx_video_data+1


;--------------------------------------------------------------------
; SGX VPC (Video Priority Controller)
    
sgx_vpcport      equ  $0008   

vpc_ctrl_1       equ  sgx_vpcport
vpc_ctrl_2       equ  sgx_vpcport+1

vpc_window_1     equ  sgx_vpcport+2   
vpc_window_1_l   equ  vpc_window_1  
vpc_window_1_h   equ  vpc_window_1+1    
    
vpc_window_2     equ  sgx_vpcport+4   
vpc_window_2_l   equ  vpc_window_2  
vpc_window_2_h   equ  vpc_window_2+1    

vpc_vdc_redir    equ  sgx_vpcport+6

;--------------------------------------------------------------------
; VCE (Video Color Encoder)

colorport        equ $0400
color_ctrl       equ  colorport

color_reg        equ  colorport+2
color_reg_l      equ  color_reg
color_reg_h      equ  color_reg+1

color_data       equ  colorport+4
color_data_l     equ  color_data
color_data_h     equ  color_data+1


;--------------------------------------------------------------------
; PSG (Programmable Sound Generator)

psgport          equ  $0800
psg_ch           equ  psgport
psg_mainvol      equ  psgport+1
psg_freqlo       equ  psgport+2
psg_freqhi       equ  psgport+3
psg_ctrl         equ  psgport+4
psg_pan          equ  psgport+5
psg_wavebuf      equ  psgport+6
psg_noise        equ  psgport+7
psg_lfofreq      equ  psgport+8
psg_lfoctrl      equ  psgport+9


;--------------------------------------------------------------------
; TIMER

timerport        equ  $0C00
timer_cnt        equ  timerport
timer_ctrl       equ  timerport+1        


;--------------------------------------------------------------------
; I/O port

joyport          equ  $1000


;--------------------------------------------------------------------
; IRQ ports

irqport          equ  $1400
irq_disable      equ  irqport+2
irq_status       equ  irqport+3

;--------------------------------------------------------------------
; CDROM/Expansion ports

cd_port          equ  $1800

bram_lock        equ  cd_port+3  ; a read access here will do it
bram_unlock      equ  cd_port+7  ; actually, bit #$80 of this byte

;--------------------------------------------------------------------
; Arcade Card ports

ac_port          equ  $1A00
ac_data1         equ  ac_port
ac_data1_alt     equ  ac_port+1
ac_base1_l       equ  ac_port+2
ac_base1_m       equ  ac_port+3
ac_base1_h       equ  ac_port+4
ac_offset1_l     equ  ac_port+5
ac_offset1_m     equ  ac_port+6
ac_cntrol1       equ  ac_port+9
ac_addoffset1    equ  ac_port+$0A

ac_shftreg       equ  $1AE0 ; actually, probably rotate register
ac_shftreg_0     equ  ac_shftreg
ac_shftreg_1     equ  ac_shftreg+1
ac_shftreg_2     equ  ac_shftreg+2
ac_shftreg_3     equ  ac_shftreg+3
ac_shft_bits     equ  ac_shftreg+4  ; positive = shift left

ac_identbase     equ  $1AFD
ac_identver_l    equ  ac_identbase
ac_identver_h    equ  ac_identbase+1
ac_identflag     equ  ac_identbase+2

AC_IDENT         equ  $51 ; if ac_identflag = AC_IDENT, then AC in use


;--------------------------------------------------------------------
; Macro equates

ARG_NONE       equ 0
ARG_REG        equ 1
ARG_IMMED      equ 2
ARG_ABS        equ 3
ARG_ABSOLUTE   equ 3
ARG_INDIRECT   equ 4
ARG_STRING     equ 5
ARG_LABEL      equ 6


;--------------------------------------------------------------------
; VDC REG equates 

MAWR      equ  $00   ;Memory Access Write Reg
MARR      equ  $01   ;Memory Access Read Reg
VRWR      equ  $02   ;Vram Read/Write reg
VWR       equ  $02   ;Vram Read/Write reg
VRR       equ  $02   ;Vram Read/Write reg
CR        equ  $05   ;Control Reg
RCR       equ  $06   ;Raster Control Reg
BXR       equ  $07   ;Background X(scroll) Reg
BYR       equ  $08   ;Background Y(scroll) Reg
MWR       equ  $09   ;Memory Access Width Reg
HSR       equ  $0a   ;Horizontal Synchro Reg
HDR       equ  $0b   ;Horizontal Display Reg
VSR       equ  $0c   ;Vertical Synchro Reg
VDR       equ  $0d   ;Vertical Display Reg
VDE       equ  $0e   ;Vertical Display End Reg
DCR       equ  $0f   ;DMA Control Reg
DSR       equ  $10   ;DMA Source Address Reg
DDR       equ  $11   ;DMA Destination Address Reg
DBR       equ  $12   ;DMA Block Length Reg
SATB      equ  $13   ;VRAM-SATB Source Address Reg 
L_RES     equ  $04   ;(5.37mhz) 256
M_RES     equ  $05   ;(7.1mhz) 352
H_RES     equ  $06   ;(10.5mhz) 512

;--------------------------------------------------------------------
;VDC ports
vdc_status   = $0000
vreg_port    = $0000
vdata_port   = $0002
vdata_port.l = $0002
vdata_port.h = $0003

;--------------------------------------------------------------------
;VDC CR reg arguments
BG_ON     = $0080
BG_OFF    = $0000
SPR_ON    = $0040
SPR_OFF   = $0000
VINT_ON   = $0008
VINT_OFF  = $0000
HINT_ON   = $0004
HINT_OFF  = $0000
ALL_OFF   = $0000 

;--------------------------------------------------------------------
;VDC vram increment
INC_1   = %00000000
INC_32  = %00001000
INC_64  = %00010000
INC_128 = %00011000

;--------------------------------------------------------------------
;VDC map sizes
SCR32_32  = %00000000 
SCR32_64  = %01000000
SCR64_32  = %00010000
SCR64_64  = %01010000
SCR128_32 = %00100000
SCR128_64 = %01100000

;--------------------------------------------------------------------
;VDC DMA control
AUTO_SATB_ON =  $0010
AUTO_SATB_OFF = $0000

;--------------------------------------------------------------------
;VDC sprite attributes
V_FLIP    = %1000000000000000
H_LFIP    = %0000100000000000
SIZE16_16 = %0000000000000000
SIZE16_32 = %0000100000000000
SIZE16_64 = %0001100000000000
SIZE32_16 = %0000000100000000
SIZE32_32 = %0000100100000000
SIZE32_64 = %0001100100000000
PRIOR_L   = %0000000000000000
PRIOR_H   = %0000000010000000
SPAL1     = %0000000000000000
SPAL2     = %0000000000000001
SPAL3     = %0000000000000010
SPAL4     = %0000000000000011
SPAL5     = %0000000000000100
SPAL6     = %0000000000000101
SPAL7     = %0000000000000110
SPAL8     = %0000000000000111
SPAL9     = %0000000000001000
SPAL10    = %0000000000001001
SPAL11    = %0000000000001010
SPAL12    = %0000000000001011
SPAL13    = %0000000000001100
SPAL14    = %0000000000001101
SPAL15    = %0000000000001110
SPAL16    = %0000000000001111



;--------------------------------------------------------------------
; VCE resolution
LO_RES   = %00000000    ;5.369mhz
MID_RES  = %00000001    ;7.159mhz
HI_RES   = %00000010    ;10.739mhz
H_FILTER = %00000100

;--------------------------------------------------------------------
;VCE ports
vce_cntrl  = $400
vce_clr    = $402
vce_clr.l  = $402
vce_clr.h  = $403
vce_data   = $404
vce_data.l = $404
vce_data.h = $405

;--------------------------------------------------------------------
;TIMER ports
TMR_CMD    = $c00
TMR_PORT   = $c01
TMR_ON     = $01
TMR_OFF    = $01


;--------------------------------------------------------------------
; IRQ mask 
IRQ2_ON =  %00000000
VIRQ_ON =  %00000000
TIRQ_ON =  %00000000
IRQ2_OFF = %00000001
VIRQ_OFF = %00000010
TIRQ_OFF = %00000100


;--------------------------------------------------------------------
; Txx
tin_DMA = $D3
tia_DMA = $E3
tii_DMA = $73
tdd_DMA = $C3
tai_DMA = $F3

;--------------------------------------------------------------------
; CD 

EX_MEMOPEN   equ  $E0DE ;SCD version check
CD_READ      equ  $E009 ;CD sector read



;--------------------------------------------------------------------
; MPR slots
MPR0 = 0
MPR1 = 1
MPR2 = 2
MPR3 = 3
MPR4 = 4
MPR5 = 5
MPR6 = 6
MPR7 = 7




