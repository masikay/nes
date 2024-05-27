.include "consts.inc"
.include "header.inc"
.include "reset.inc"

;;-----------------------------------------------------------------------------
;; PRG-ROM code located at $8000
;;-----------------------------------------------------------------------------
.segment "CODE"

.proc LoadPalette
	ldy #0
LoopPalette:
	lda PaletteData,y				; Lookup byte in ROM
	sta PPU_DATA					; Set the value to send to PPU_DATA (PPU_ADDR is inremented automatically)
	iny								; Y++
	cpy #32							; is Y == 32?
	bne LoopPalette					; Not yet, keep looping
	rts
.endproc

;;-----------------------------------------------------------------------------
;; Reset handler (called when the NES resets or powers on)
;;-----------------------------------------------------------------------------
Reset:
	INIT_NES

Main:
	;;-----------------------------------------------------------------------------
	;; Set the main background color
	;;-----------------------------------------------------------------------------
	bit PPU_STATUS					; Straw PPU_STATUS to reset the PPU_ADDR latch
	ldx #$3F
	stx PPU_ADDR					; Set hi-byte of PPU_ADDR to $3F
	ldx $00
	stx PPU_ADDR					; Set hi-byte of PPU_ADDR to $00

	jsr LoadPalette					; Jump to subroutine LoadPalette
	
	lda #%00011110
	sta PPU_MASK					; Set PPU_MASK bits to show background


LoopForever:
	jmp LoopForever

NMI:
	rti

IRQ:
	rti

PaletteData:
.byte $0F,$2A,$0C,$3A, $0F,$2A,$0C,$3A, $0F,$2A,$0C,$3A, $0F,$2A,$0C,$3A	; Background
.byte $0F,$10,$00,$26, $0F,$10,$00,$26, $0F,$10,$00,$26, $0F,$10,$00,$26	; Sprites

.segment "VECTORS"
.word NMI						; Address of the NMI handler
.word Reset						; Address of the RESET handler
.word IRQ						; Address of the IRQ handler