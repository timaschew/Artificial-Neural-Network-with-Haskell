# configuration
COMP		= ghc
COMP_OPT	= -O2 -threaded	# -auto-all -prof --fforce-recomp -Wall
RUN_OPT		= +RTS -N2 	# -H1024M -prof -sstderr
BIN		= Main Gui


# actions
run1:
	./Main demo 500
	
run2:
	./Main demo 1000

run3:
	./Main pgm1 10

xor:
	./Main demoXor 2000

gui:	Gui.hs
	$(COMP) $(COMP_OPT) --make $+ 
	./Gui $(RUN_OPT)

# run default action
run:
	make clean && make gui
#	make clean && make compile && make run2

compile: Main.hs
	$(COMP) $(COMP_OPT) --make $+

clean:
	touch $(BIN) temp.o temp.hi && rm *.o *.hi $(BIN)
