CA = ca65.exe
CC = cc65.exe
LD = ld65.exe

CONFIG_FLAG = ../nes.cfg

OBJ = exercise-01.o \
exercise-02.o \
exercise-03.o \
exercise-04.o \
exercise-05.o \
exercise-06.o \
exercise-07.o \
exercise-08.o \
exercise-09.o

ROM = exercise-01.nes \
exercise-02.nes \
exercise-03.nes \
exercise-04.nes \
exercise-05.nes \
exercise-06.nes \
exercise-07.nes \
exercise-08.nes \
exercise-09.nes

# -----------------------------------------------------------------------------
# Rule to assemble and link all assembly files
# -----------------------------------------------------------------------------
build: $(ROM)

%.o : %.asm
	$(CA) $< -o $@

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