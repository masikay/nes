CA = ca65.exe
CC = cc65.exe
LD = ld65.exe

CONFIG_FLAG = ../nes.cfg
INCLUDE_DIR = ../include


OBJ = hellogoomba.o
ROM = hellogoomba.nes

# -----------------------------------------------------------------------------
# Rule to assemble and link all assembly files
# -----------------------------------------------------------------------------
build: $(ROM)

%.o : %.asm
	$(CA) -I $(INCLUDE_DIR) $< -o $@

%.nes : %.o
	$(LD) -C $(CONFIG_FLAG) $< -o $@


# -----------------------------------------------------------------------------
# Rule to remove all object (.o) files and cartridge (.nes) files
# -----------------------------------------------------------------------------
clean:
	rm *.o *.nes

# -----------------------------------------------------------------------------
# Rule to run the final cartridge (.nes) file in the FCEUX emulator
# -----------------------------------------------------------------------------
run:
	fceux.exe $(ROM)