all:
	happy -gca ParMini.y
	alex -g LexMini.x
	ghc --make TestMini.hs -o TestMini

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocMini.* LexMini.* ParMini.* LayoutMini.* SkelMini.* PrintMini.* TestMini.* AbsMini.* TestMini ErrM.* SharedString.* ComposOp.* mini.dtd XMLMini.* Makefile*
	

