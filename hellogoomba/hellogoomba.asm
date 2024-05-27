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
;; Subroutine to load all 32 bytes into OAM-RAM starting at $0200
;;-----------------------------------------------------------------------------
.proc LoadSprites
	ldx #0
LoopSprites:
 	lda SpriteData,x		; Fetch bytes from the SpriteData lookup table
	sta $0200,x				; Store the bytes starting at OAM address $0200
 	inx						; X++
	cpx #32
	bne LoopSprites			; Loop 32 times (4 hardware sprites, 4 bytes each)
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
	jsr LoadPalette			; Call LoadPalette subroutine to load 32 colors into our palette
	jsr LoadBackground		; Call LoadBackground subroutine to load a full nametable of tiles and attributes
	jsr LoadSprites			; Call LoadSprites subroutine to load all sprites into OAM-RAM

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

	lda #$02				; For every frame, copy the data starting at $02**
	sta $4014				; The OAM DMA copy starts when write in PPU register mapped at $4014 
	
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
.incbin "../assets/graphics/mario_background.nam"

;;-----------------------------------------------------------------------------
;; This is the OAM sprite attribute data that will be used in the game.
;; There is only one big metasprite that is composed of 4 hardware sprites.
;; The OAM is organized in sets of 4 bytes per tile.
;;-----------------------------------------------------------------------------
SpriteData:

;--------------------------------
; Mario
;      Y  tile# attributes	X
.byte $AF, $3A, %00000000, $98	; OAM sprite 0 at (x: 16, y: 16)
.byte $AF, $37, %00000000, $A0	; OAM sprite 1 at (x: 24, y: 16)
.byte $B7, $4F, %00000000, $98	; OAM sprite 2 at (x: 16, y: 24)
.byte $B7, $4F, %01000000, $A0	; OAM sprite 3 at (x: 24, y: 24) --> flip-horz

;--------------------------------
; Goomba
;      Y  tile# attributes	X
.byte $93, $70, %00100011, $C7	; OAM sprite 0 at (x: 199, y: 147)
.byte $93, $71, %00100011, $CF	; OAM sprite 1 at (x: 207, y: 147)
.byte $9B, $72, %00100011, $C7	; OAM sprite 2 at (x: 199, y: 155)
.byte $9B, $73, %00100011, $CF	; OAM sprite 3 at (x: 207, y: 155)


; Sprite Attribute Byte:
;-----------------------
; 76543210
; |||   ||
; |||   ++- Color Palette of sprite. Choose which set of 4 from the 16 colors to use
; |||
; ||+------ Priority (0: in front of background; 1: behind background)
; |+------- Flip sprite horizontally
; +-------- Flip sprite vertically

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
