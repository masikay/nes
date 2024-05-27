;;-----------------------------------------------------------------------------
;; The iNES header (contains a total of 16 bytes, flags at position $7FF0)
;;-----------------------------------------------------------------------------
.segment "HEADER"
.byte $4E,$45,$53,$1A			; 4 bytes with the characters 'N','E','S','\n'
.byte $02						; How many 16KB of PRG-ROM are used (2x16 = 32KB)
.byte $01						; How many 8KB of CHR-ROM are used (1x8 = 8KB)
.byte %00000000					; Flags 6: Horiz. mirroring, no battery, mapper 0
.byte %00000000					; Flags 7: mapper 0, playchoice, NES 2.0
.byte $00						; Flags 8: No PRG-RAM
.byte $00						; Flags 9: NTSC TV format
.byte $00						; Flags 10: No PRG-RAM
.byte $00,$00,$00,$00,$00		; Flags 11-15: Unused padding to complete 16 bytes of header

;;-----------------------------------------------------------------------------
;; PRG-ROM code located at $8000
;;-----------------------------------------------------------------------------
.segment "CODE"

Reset:
	sei							; Disable all IRQ requests
	cld							; Clear the decimal mode (unsupported by NES)
	ldx #$FF
	txs							; Initialize the stack pointer at $01FF

	inx							; Increment X, causing a roll-off from $FF to $00

	txa							; A = 0
ClearRAM:
	sta $0000,x					; Zero RAM address from $0000 to $00FF
	sta $0100,x					; Zero RAM address from $0100 to $01FF
	sta $0200,x					; Zero RAM address from $0200 to $02FF
	sta $0300,x					; Zero RAM address from $0300 to $03FF
	sta $0400,x					; Zero RAM address from $0400 to $04FF
	sta $0500,x					; Zero RAM address from $0500 to $05FF
	sta $0600,x					; Zero RAM address from $0600 to $06FF
	sta $0700,x					; Zero RAM address from $0700 to $07FF
	inx							; X++
	bne ClearRAM				; Loops until X reaches 0 again (after roll-off)

LoopForever:
	jmp LoopForever

NMI:
	rti

IRQ:
	rti

.segment "VECTORS"
.word NMI						; Address of the NMI handler
.word Reset						; Address of the RESET handler
.word IRQ						; Address of the IRQ handler