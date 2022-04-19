bin/main:
	ghc -o bin/main -odir bin -hidir bin src/Main.hs
	
clean:
	rm bin/*

.PHONY: clean
