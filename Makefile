SRC=src
GRAM=$(SRC)/Gramatyka
LEX=$(GRAM)/LexLatte
PAR=$(GRAM)/ParLatte
MAIN_SRC_FILE=$(SRC)/latc.hs
TARGET_FILE=latc
all: $(TARGET_FILE)

$(PAR).hs: $(PAR).y
	happy -gca $^ -o $@

$(LEX).hs: $(LEX).x
	alex --ghc $^ -o $@


$(TARGET_FILE): $(PAR).hs $(LEX).hs $(MAIN_SRC_FILE)
	ghc -isrc -Wall --make $(MAIN_SRC_FILE) -o $(TARGET_FILE)

clean:
	rm -f $(GRAM)/*.{o,hi} $(SRC)/latc.{hi,o} $(SRC)/$(TARGET_FILE) $(TARGET_FILE)