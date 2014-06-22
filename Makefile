prefix := /usr/local
BIN  = gcal2org
TEST = test

$(BIN): Main.hs
	ghc --make -O $< -o $@
$(TEST): Test.hs
	ghc --make -main-is Test.tests $< -o $(TEST)

.PHONY: clean install check
clean:
	$(RM) $(BIN) $(BIN)~ *.hi *.o $(TEST)
install: $(BIN)
	gzexe $(BIN) && install -m 555 $(BIN) $(prefix)/bin
check: $(TEST)
	$(CURDIR)/$(TEST)
