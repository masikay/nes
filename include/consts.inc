;;-----------------------------------------------------------------------------
;; Constants for PPU registers mapper from address $200 to $2007
;;-----------------------------------------------------------------------------
PPU_CTRL		= $2000
PPU_MASK		= $2001
PPU_STATUS		= $2002
OAM_ADDR		= $2003
OAM_DATA		= $2004
PPU_SCROLL		= $2005
PPU_ADDR		= $2006
PPU_DATA		= $2007
PPU_OAM_DMA		= $4014

;;-----------------------------------------------------------------------------
;; Controller ports and buttons
;;-----------------------------------------------------------------------------
JOYPAD1			= $4016
JOYPAD2			= $4017

BUTTON_A		= $80
BUTTON_B		= $40
BUTTON_SELECT	= $20
BUTTON_START	= $10
BUTTON_UP		= $08
BUTTON_DOWN		= $04
BUTTON_LEFT		= $02
BUTTON_RIGHT	= $01