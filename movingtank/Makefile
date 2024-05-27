CA = ca65.exe
CC = cc65.exe
LD = ld65.exe
PY = python3

FCEUX_SYMBOLS = ../scripts/fceux_debug_sysmbols.py

CONFIG_FLAG = ../nes.cfg
INCLUDE_DIR = ../include


OBJ = movingtank.o
ROM = movingtank.nes
BASE_FILE = $(basename $(ROM))

# -----------------------------------------------------------------------------
# Rule to assemble and link all assembly files
# -----------------------------------------------------------------------------
build: $(ROM)

%.o : %.asm
	$(CA) -I $(INCLUDE_DIR) $< -g -o $@

%.nes : %.o
	$(LD) -C $(CONFIG_FLAG) $< -o $@ -m $(BASE_FILE).map.txt -Ln  $(BASE_FILE).label.txt --dbgfile $(BASE_FILE).nes.dbg
	$(PY) $(FCEUX_SYMBOLS) $(BASE_FILE).label.txt

# -----------------------------------------------------------------------------
# Rule to remove all object (.o) files and cartridge (.nes) files
# -----------------------------------------------------------------------------
clean:
	rm *.nes *.txt *.dbg *.nl

# -----------------------------------------------------------------------------
# Rule to run the final cartridge (.nes) file in the FCEUX emulator
# -----------------------------------------------------------------------------
run:
	fceux.exe $(ROM)