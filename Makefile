all: mymvm myass
myass: myass.hs
	ghc myass/Main.hs
mymvm: mymvm.c
	gcc -std=gnu11 mymvm.c -o mymvm
