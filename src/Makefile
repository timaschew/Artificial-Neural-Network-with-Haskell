ref:
	./Main "ref" 100

run1:
#	./Main +RTS -prof
	./Main "demo2" 500 +RTS -H256M -prof
	
run2:
#	./Main "demo2" 500 +RTS -H256M -prof
	./Main "demo2" 1000 +RTS -H256M -prof -sstderr

run:
	make clean && make compile && make run2

compile:
	ghc -O2 --make Main.hs -prof -auto-all

clean:
	rm *.o *.hi Main
