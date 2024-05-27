.include "consts.inc"
.include "header.inc"
.include "reset.inc"
.include "utils.inc"


.segment "ZEROPAGE"
Frame:		.res 1			; Reserve 1 byte to store the numebr of frame
Clock60:	.res 1			; Reserve 1 byte to store a counter that increments every second (60 frames)
BgPtr:		.res 2			; Reserve 2 bytes (16 bits) to store a pointer to the background address
							; (store first the lo-byte, then the hi-byte) - little endian

;;-----------------------------------------------------------------------------
;; PRG-ROM code located at $8000
;;-----------------------------------------------------------------------------
.segment "CODE"

;;-----------------------------------------------------------------------------
;; Subroutine to load all 32 color palette values from ROM
;;-----------------------------------------------------------------------------
.proc LoadPalette
	PPU_SETADDR $3F00
	ldy #0					; Y = 0
:	lda PaletteData,y		; Lookup byte in ROM
	sta PPU_DATA			; Set value to send to PPU_DATA
	iny						; Y++
	cpy #32					; Is Y equal to 32?
	bne :-					; Not yet, keep looping
	rts						; Return from subroutine
.endproc

;;-----------------------------------------------------------------------------
;; Subroutine to load tiles and attributes into the first nametable
;;-----------------------------------------------------------------------------
.proc LoadBackground
	lda #<BackgroundData	; Fetch the lo-byte of BackgroundData address
	sta BgPtr
	lda #>BackgroundData	; Fetch the hi-byte of BackgroundData address
	sta BgPtr + 1

	PPU_SETADDR $2000

	ldx #0					; X = 0 for the OuterLoop (hi-byte from $00 to $4)
	ldy #0					; Y = 0 for the InnerLoop (lo-byte from $00 to $FF)

OuterLoop:	
InnerLoop:
	lda (BgPtr),y			; Lookup byte in ROM
	sta PPU_DATA			; Set value to send to PPU_DATA
	iny						; Y++
	cpy #0					; Is Y == 0 (wrapped around 256 times)?
	beq IncreaseHiByte		; Then: increase the hi-byte
	jmp InnerLoop			; Else: continue the inner loop
IncreaseHiByte:
	inc BgPtr+1				; Increment the hi-byte pointer to the next background section (next 255-chunks)
	inx						; X++
	cpx #4					; Is x == 4?
	bne OuterLoop			; If not, keep lloping back to the outer loop

	rts						; Return from subroutine
.endproc

;;-----------------------------------------------------------------------------
;; Subroutine to load text in the nametable until it finds a 0-terminator
;;-----------------------------------------------------------------------------
.proc LoadText
	PPU_SETADDR $21CB		; Set the nametable position where the text starts

	ldy #0					; Y = 0
Loop:
	lda TextMessage,y		; Fetch character byte from ROM
	beq EndLoop				; If the character is 0, then exit loop

	cmp #32					; Compare the loaded character to ASCII #32 (space)
	bne DrawLetter			; If not space, then proceed to draw a letter character
DrawSpace:
	lda #$24				; Tile $24 is an empty tile
	sta PPU_DATA			; Store PPU_DATA with the empty tile
	jmp NextChar			; Proceed to the next character
DrawLetter:
	sec
	sbc #55					; Subtract 55 to map the byte to the correct CHR tile
	sta PPU_DATA			; Store data and advance PPU address
NextChar:
	iny						; Y++
	jmp Loop				; Continue looping since we are still not done

EndLoop:
	rts						; Return from subroutine
.endproc

;;-----------------------------------------------------------------------------
;; Reset handler (called when the NES resets or powers on)
;;-----------------------------------------------------------------------------
Reset:
	INIT_NES				; Macro to initialize the NES to a known state

	lda #0
	sta Frame
	sta Clock60

Main:
	jsr LoadPalette			; Call the subroutine to load 32 colors into the palette
	jsr LoadBackground		; Call the subroutine to load a full nametable of tiles and attributes
	jsr LoadText			; Call LoadText subroutine to draw the text message on the nametable

EnablePPURendering:
	lda #%10010000			; Enable NMI and set background to use the 2nd pattern table (at $1000)
	sta PPU_CTRL
	lda #0
	sta PPU_SCROLL			; Disable scroll in X
	sta PPU_SCROLL			; Disable scroll in Y
	lda #%00011110
	sta PPU_MASK			; Set PPU_MASK bits to render the background

LoopForever:
	jmp LoopForever			; Force an infinite execution loop

;;-----------------------------------------------------------------------------
;; NMI interrupt handler
;;-----------------------------------------------------------------------------
NMI:
	inc Frame				; Frame++

	lda Frame
	cmp #60					; Compare Frame with 60
	bne Skip				; If its' not 60, bypass ...
	inc Clock60				; Else, increment Clock60 and zero the Frame counter
	lda #0
	sta Frame
Skip:	
	rti						; Return from interrupt

;;-----------------------------------------------------------------------------
;; IRQ interrupt handler
;;-----------------------------------------------------------------------------
IRQ:
	rti						; Return from interrupt

;;-----------------------------------------------------------------------------
;; Hardcoded list of color values in ROM to be loaded by the PPU
;;-----------------------------------------------------------------------------
PaletteData:
.byte $22,$29,$1A,$0F, $22,$36,$17,$0F, $22,$30,$21,$0F, $22,$27,$17,$0F	; Background palette
.byte $22,$16,$27,$18, $22,$1A,$30,$27, $22,$16,$30,$27, $22,$0F,$36,$17	; Sprite palette

;;-----------------------------------------------------------------------------
;; Background data with tile numbers that must be copied to the nametable
;;-----------------------------------------------------------------------------
BackgroundData:
.byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
.byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
.byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
.byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
.byte $24,$24,$24,$24,$24,$36,$37,$36,$37,$36,$37,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
.byte $24,$24,$24,$24,$35,$25,$25,$25,$25,$25,$25,$38,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$36,$37,$24,$24,$24,$24,$24
.byte $24,$24,$24,$24,$39,$3A,$3B,$3A,$3B,$3A,$3B,$3C,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$35,$25,$25,$38,$24,$24,$24,$24
.byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$39,$3A,$3B,$3C,$24,$24,$24,$24
.byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
.byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
.byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$53,$54,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
.byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
.byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
.byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
.byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
.byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
.byte $24,$24,$53,$54,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45,$53,$54,$45,$45,$53,$54,$45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
.byte $24,$24,$55,$56,$24,$24,$24,$24,$24,$24,$24,$24,$47,$47,$55,$56,$47,$47,$55,$56,$47,$47,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
.byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
.byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
.byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$60,$61,$62,$63,$24,$24,$24,$24
.byte $24,$24,$24,$24,$24,$24,$31,$32,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$64,$65,$66,$67,$24,$24,$24,$24
.byte $24,$24,$24,$24,$24,$30,$26,$34,$33,$24,$24,$24,$24,$36,$37,$36,$37,$24,$24,$24,$24,$24,$24,$24,$68,$69,$26,$6A,$24,$24,$24,$24
.byte $24,$24,$24,$24,$30,$26,$26,$26,$26,$33,$24,$24,$35,$25,$25,$25,$25,$38,$24,$24,$24,$24,$24,$24,$68,$69,$26,$6A,$24,$24,$24,$24
.byte $B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5
.byte $B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7
.byte $B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B6
.byte $B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7
.byte $B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5
.byte $B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7

;;-----------------------------------------------------------------------------
;; Attributes tell which palette is used by a group of tiles in the nametable
;;-----------------------------------------------------------------------------
AttributeData:
.byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
.byte %00000000, %10101010, %10101010, %00000000, %00000000, %00000000, %10101010, %00000000
.byte %00000000, %00000000, %00000000, %00000000, %11111111, %00000000, %00000000, %00000000
.byte %00000000, %00000000, %10101010, %10101010, %10101010, %10101010, %00000000, %00000000
.byte %11111111, %00000000, %00000000, %00001111, %00001111, %00000011, %00000000, %00000000
.byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
.byte %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111
.byte %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111

TextMessage:
.byte "HELLO WORLD", $0

;;-----------------------------------------------------------------------------
;; Here we add the CHR-ROM data, included from an external .CHR file
;;-----------------------------------------------------------------------------
.segment "CHARS"
.incbin "../assets/graphics/mario.chr"

;;-----------------------------------------------------------------------------
;; Vectors with the addresses of the handlers that we always add at $FFFA
;;-----------------------------------------------------------------------------
.segment "VECTORS"
.word NMI					; Address (2 bytes) of the NMI handler
.word Reset 				; Address (2 bytes) of the Reset handler
.word IRQ					; Address (2 bytes) of the IRQ handler
