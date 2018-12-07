all: latteM compiler rights

latteM:
	cp src/Latte.cf latte/Latte.cf
	cd latte && bnfc -m Latte.cf
	$(MAKE) -C latte

compiler:
	ghc src/Main.hs -o build/Compiler -odir build -isrc -iinstant
	mv src/*.hi build

rights:
	chmod +x latc_x86_64
	chmod +x latc
	chmod +x tester

clean:
	rm -f latte/*
	rm -f build/*

.PHONY: all
