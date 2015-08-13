all: mymvm myass
myass: src/myass/Main.hs src/myass/Translate.hs src/myass/Types.hs src/myass/Parser.hs 
	ghc src/myass/Main.hs -isrc/myass
mymvm: mymvm.c
	gcc -std=gnu11 mymvm.c -o mymvm
