# configuration
COMP		= ghc
COMP_OPT	= -O2 -threaded	# -auto-all -prof --fforce-recomp -Wall
RUN_OPT		= +RTS -N2 	# -H1024M -prof -sstderr
BIN		= Main Gui


# Gui
gui:	Gui.hs
	$(COMP) $(COMP_OPT) --make $+ 
	./Gui $(RUN_OPT)

# Main
run1:	Main.hs
	$(COMP) $(COMP_OPT) --make $+
	./Main demo 500
	
run2:	Main.hs
	$(COMP) $(COMP_OPT) --make $+
	./Main demo 1000

run3:	Main.hs
	$(COMP) $(COMP_OPT) --make $+
	./Main pgm1 10

xor:	Main.hs
	$(COMP) $(COMP_OPT) --make $+
	./Main demoXor 2000

clean:
	touch $(BIN) temp.o temp.hi && rm *.o *.hi $(BIN)
