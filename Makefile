all: latteM runtime compiler rights

bnfc: bnfc_download bnfc_install

bnfc_download:
	wget https://github.com/mlazowik/bnfc/archive/176-source-position.zip
	unzip 176-source-position.zip
	rm 176-source-position.zip
	cd bnfc-176-source-position/source && cabal install

bnfc_install:
	cd bnfc-176-source-position/source && cabal install

bnfc_binary = ../bnfc-176-source-position/source/dist/build/bnfc/bnfc

latteM:
	cp src/Latte.cf latte/Latte.cf
	cd latte && $(bnfc_binary) --functor -m Latte.cf
	$(MAKE) -C latte

runtime:
	gcc -m32 -S -o build/runtime.s src/runtime.c
	gcc -m32 -o lib/runtime.o -c build/runtime.s
	rm build/runtime.s

compiler:
	ghc src/Main.hs -o build/Compiler -odir build -isrc -ilatte
	mv src/*.hi build

rights:
	chmod +x latc_x86

clean:
	rm -f latte/*
	rm -f build/*

.PHONY: all
