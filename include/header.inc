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
