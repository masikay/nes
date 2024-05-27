.include "consts.inc"
.include "header.inc"
.include "reset.inc"
.include "utils.inc"


.segment "ZEROPAGE"
Buttons:	.res 1			; Reserve 1 byte to store the controller's buttons status

XPos:		.res 2			; Player X position (8.8 foixed-point math) - Xhi + Xlo/256 pixels
YPos:		.res 2			; Player Y position (8.8 foixed-point math) - Yhi + Ylo/256 pixels

XVel:		.res 1			; Player X (signed) velocity in pixel per 256 frames
YVel:		.res 1			; Player Y (signed) velocity in pixel per 256 frames

Frame:		.res 1			; Reserve 1 byte to store the number of frame
Clock60:	.res 1			; Reserve 1 byte to store a counter that increments every second (60 frames)
BgPtr:		.res 2			; Reserve 2 bytes (16 bits) to store a pointer to the background address
							; (store first the lo-byte, then the hi-byte) - little endian

;;-----------------------------------------------------------------------------
;; Constants for player movement
;; PS: PAL frames runs ~20% slower than NTSC frames. Adjust accordingly! 
;;-----------------------------------------------------------------------------
MAXSPEED	= 120			; Max speed limit in 1/256 px/frame
ACCEL		= 2				; Movement acceleration in 1/256 px/frame^2
BRAKE		= 4				; Stopping acceleration in 1/256 px/frame^2

;;-----------------------------------------------------------------------------
;; PRG-ROM code located at $8000
;;-----------------------------------------------------------------------------
.segment "CODE"

;;-----------------------------------------------------------------------------
;; Subroutine to read controller state and to store it inside "Buttons" in RAM
;;-----------------------------------------------------------------------------
.proc ReadControllers
	lda #1					; A++
	sta Buttons				; Buttons = 1

	sta JOYPAD1				; Set Latch = 1 to begin controller Input mode
	lsr 					; A = 0
	sta JOYPAD1				; Set Latch = 1 to begin controller Ouput mode

LoopButtons:
	lda JOYPAD1				; Read from controller
	lsr						; Move the 1st bit to Carry
	rol Buttons				; Load the Carry value in Buttons
	bcc	LoopButtons			; Loop until CArry is set ( = 1 ) due to the the the initial value of Buttons

	rts
.endproc

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

InitVariables:
	lda #0
	sta Frame				; Frame = 0
	sta Clock60				; Clock60 = 0

	lda #20
	sta XVel				; XVel is 20 pixels per 256 frames

	
	ldx #0
	lda SpriteData,x
	sta YPos + 1			; Set Y position hi-byte
	inx
	inx
	inx
	lda SpriteData,x
	sta XPos + 1			; Set X position hi-byte

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

OAMStartDMACopy:			; DMA copy of OAM data from RAM to PPU
	lda #$02				; For every frame, copy the data starting at $02**
	sta PPU_OAM_DMA			; The OAM DMA copy starts when write in PPU register mapped at $4014

ConmtrollerInput:
	jsr ReadControllers		; Read the controller buttons
	
CheckRightButton:
	lda Buttons
	and #BUTTON_RIGHT		; Check if the right button is pressed (%00000001)
	beq NotRight
		;; Right pressed
		lda XVel
		clc
		adc #ACCEL			; Add the acceleration to the velocity
		cmp #MAXSPEED		; Check if the max velocity was reached
		bcc :+
			lda #MAXSPEED	; Forcing the new velocity to be the MAXSPEED
		:
		sta XVel
		jmp CheckLeftButton
	NotRight:
		lda XVel
		cmp #BRAKE			; Check if BRAKE can be subtracted from the velocity
		bcs :+
			lda #BRAKE + 1	; Force it to be the BRAKE (+1 to compensate for the carry)
		:
		sbc #BRAKE			; Subtract the brake (deceleration)
		sta XVel			; Save the new updated velocity

CheckLeftButton:
	;; TODO: ...
CheckDownButton:
	;; TODO: ...
CheckUpButton:
	;; TODO: ...

EndInputCheck:

UpdateSpritePosition:
	;Update position based on velocity
	lda XVel
	clc
	adc XPos				; Add the velocity to the XPos lo-byte
	sta XPos
	lda #0
	adc XPos + 1			; Add the hi-byte (using the carry from the previous add)
	sta XPos + 1


DrawSpriteTile:

	lda XPos + 1
	sta $0203				; Set the 1st sprite X position (XPos)
	sta $020B				; Set the 3rd sprite X position (XPos)
	clc
	adc #8
	sta $0207				; Set the 2nd sprite X position (XPos + 8)
	sta $020F				; Set the 4th sprite X position (XPos + 8)

	lda YPos + 1
	sta $0200				; Set the 1st sprite Y position (YPos)
	sta $0204				; Set the 2nd sprite Y position (YPos)
	clc
	adc #8
	sta $0208				; Set the 3rd sprite Y position (YPos + 8)
	sta $020C				; Set the 4th sprite Y position (YPos + 8)

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
.byte $1D,$10,$20,$2D, $1D,$1D,$2D,$10, $1D,$0C,$19,$1D, $1D,$06,$17,$07	; Background palette
.byte $0F,$1D,$19,$29, $0F,$08,$18,$38, $0F,$0C,$1C,$3C, $0F,$2D,$10,$30	; Sprite palette

;;-----------------------------------------------------------------------------
;; Background data with tile numbers that must be copied to the nametable
;;-----------------------------------------------------------------------------
BackgroundData:
.incbin "../assets/graphics/battle_background.nam"

;;-----------------------------------------------------------------------------
;; This is the OAM sprite attribute data that will be used in the game.
;; There is only one big metasprite that is composed of 4 hardware sprites.
;; The OAM is organized in sets of 4 bytes per tile.
;;-----------------------------------------------------------------------------
SpriteData:
;      Y  tile# attributes	X
.byte $80, $18, %00000000, $10	; OAM sprite 0
.byte $80, $1A, %00000000, $18	; OAM sprite 1
.byte $88, $19, %00000000, $10	; OAM sprite 2
.byte $88, $1B, %00000000, $18	; OAM sprite 3

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
.incbin "../assets/graphics/battle.chr"

;;-----------------------------------------------------------------------------
;; Vectors with the addresses of the handlers that we always add at $FFFA
;;-----------------------------------------------------------------------------
.segment "VECTORS"
.word NMI					; Address (2 bytes) of the NMI handler
.word Reset 				; Address (2 bytes) of the Reset handler
.word IRQ					; Address (2 bytes) of the IRQ handler
