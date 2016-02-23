SRC=src
GRAM=$(SRC)/Gramatyka
LEX=$(GRAM)/LexLatte
PAR=$(GRAM)/ParLatte
MAIN_SRC_FILE=$(SRC)/latc.hs
TARGET_FILE=lib/latc
all: $(TARGET_FILE) lib/lib.o

$(PAR).hs: $(PAR).y
	happy -gca $^ -o $@

$(LEX).hs: $(LEX).x
	alex --ghc $^ -o $@


$(TARGET_FILE): $(PAR).hs $(LEX).hs $(MAIN_SRC_FILE) $(SRC)/*.hs
	ghc -j9 -isrc -Wall --make $(MAIN_SRC_FILE) -o $(TARGET_FILE)

clean:
	rm -f $(GRAM)/*.{o,hi} $(SRC)/latc.{hi,o} $(SRC)/$(TARGET_FILE) $(TARGET_FILE)

tests: $(TARGET_FILE)
	./test.sh

test:
	make tests

compile: latc core010.lat
	./latc core010.lat > core010.s
	nasm -f elf32 core010.s -o core010.o
	ld -L /usr/lib32 --dynamic-linker=/lib/ld-linux.so.2 -lc -melf_i386 -o core010 core010.o lib.o

redo:
	nasm -f elf32 core010.s -o core010.o
	gcc -m32 core010.o lib.o -o core010

lib/lib.o: $(SRC)/lib.c
	gcc -m32 -c -g $^ -o $@
#	nasm -f elf32 -g -F dwarf lib/lib.asm -o lib/lib.o

.PHONY:	lib redo compile test tests clean all

lib:
	make lib/lib.o
