.macro PPU_SETADDR addr
    bit PPU_STATUS				; Read PPU_STATUS to reset the PPU_ADDR latch
    ldx #>addr					; Fetch the hi-byte from 16-bit address
    stx PPU_ADDR				; Store the hi-byte of PPU_ADDR $2600
    ldx #<addr					; Fetch the lo-byte from 16-bit address
    stx PPU_ADDR				; Store the lo-byte of PPU_ADDR $2600
.endmacro

.macro PPU_SETDATA val
	lda val
	sta PPU_DATA				; Send value to PPU register $2007
.endmacro