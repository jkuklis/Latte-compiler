all: compiler

true_all: bnfc_download latteM compiler rights

bnfc_download:
	wget https://github.com/mlazowik/bnfc/archive/176-source-position.zip
	unzip 176-source-position.zip
	rm 176-source-position.zip
	cd bnfc-176-source-position/source && cabal install

bnfc_install:
	cd bnfc-176-source-position/source && cabal install

bnfc_binary = bnfc-176-source-position/source/dist/build/bnfc/bnfc

latteM:
	cp src/Latte.cf latte/Latte.cf
	cd latte && ../$(bnfc_binary) --functor -m Latte.cf
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
