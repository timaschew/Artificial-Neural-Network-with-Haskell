# configuration
COMP		= ghc
COMP_OPT	= -O2 -auto-all -prof -threaded	# --fforce-recomp -Wall
RUN		= ./Main
RUN_OPT		= +RTS -N2 -H1024M -prof -sstderr
BIN		= Main Gui


# actions
run1:
	$(RUN) demo 500
	
run2:
	$(RUN) demo 1000

run3:
	$(RUN) pgm1 10

xor:
	$(RUN) demoXor 2000

gui:	Gui.hs
	$(COMP) --make $+ && ./Gui $(RUN_OPT)

# run default action
run:
	make clean && make gui
#	make clean && make compile && make run2

compile: Main.hs
	$(COMP) $(COMP_OPT) --make Main.hs 

clean:
	touch $(BIN) temp.o temp.hi && rm *.o *.hi $(BIN)
