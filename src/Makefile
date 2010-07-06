# configuration
COMP		= ghc
COMP_OPT	= -O2 -auto-all -prof	# --fforce-recomp -Wall
RUN		= ./Main
RUN_OPT		= +RTS -H1024M -prof -sstderr
BIN		= Main Gui


# actions
run1:
	$(RUN) demo2 500 $(RUN_OPT)
	
run2:
#	./Main demo2 1000 +RTS -H256M -prof -sstderr
	$(RUN) demo2 1000 $(RUN_OPT)

run3:
	$(RUN) pgm1 10 $(RUN_OPT)

ref:
	$(RUN) ref 100

gui:	Gui.hs
	$(COMP) --make $+ && ./Gui

# run default action
run:
	make clean && make compile && make run2

compile: Main.hs
	$(COMP) $(COMP_OPT) --make Main.hs 
#	ghc -O2 --make Main.hs -prof -auto-all

clean:
	touch Main Gui && rm *.o *.hi $(BIN)
