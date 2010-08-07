# configuration
COMP		= ghc
COMP_OPT	= -O2 -auto-all -prof -threaded	# --fforce-recomp -Wall
RUN		= ./Main
RUN_OPT		= +RTS -N2 -H1024M -prof -sstderr
BIN		= Main Gui


# actions
run1:
	$(RUN) demo2 500 $(RUN_OPT)
	
run2:
	$(RUN) demo2 1000 $(RUN_OPT)

run3:
	$(RUN) pgm1 10 $(RUN_OPT)

ref:
	$(RUN) ref 100

gui:	Gui.hs
	$(COMP) --make $+ && ./Gui

# run default action
run:
	make clean && make gui
#	make clean && make compile && make run2

compile: Main.hs
	$(COMP) $(COMP_OPT) --make Main.hs 

clean:
	touch $(BIN) temp.o temp.hi && rm *.o *.hi $(BIN)
