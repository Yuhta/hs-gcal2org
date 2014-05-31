prefix := /usr/local
BIN = gcal2org

$(BIN): Main.hs
	ghc --make -O $< -o $@

.PHONY: clean install
clean:
	$(RM) $(BIN) $(BIN)~ *.hi *.o
install:
	gzexe $(BIN) && install -m 555 $(BIN) $(prefix)/bin
