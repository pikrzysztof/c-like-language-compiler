all:
	happy -gca Gramatyka/ParLatte.y
	alex -g Gramatyka/LexLatte.x
	ghc --make -Wall latc.hs -o latc

clean:
	-rm -f Gramatyka/*.log Gramatyka/*.aux Gramatyka/*.hi Gramatyka/*.o Gramatyka/*.dvi *.hi *.o
	rm latc.hi ../latc latc.o latc
	-rm -f Gramatyka/DocLatte.ps

distclean: clean
	-rm -f Gramatyka/DocLatte.* Gramatyka/LexLatte.* Gramatyka/ParLatte.* Gramatyka/LayoutLatte.* Gramatyka/SkelLatte.* Gramatyka/PrintLatte.* Gramatyka/TestLatte.* Gramatyka/AbsLatte.* Gramatyka/TestLatte Gramatyka/ErrM.* Gramatyka/SharedString.* Gramatyka/ComposOp.* Gramatyka/Latte.dtd Gramatyka/XMLLatte.* Makefile*
		-rmdir -p Gramatyka/


konflikty:
	bnfc --haskell-gadt -p Gramatyka Latte.cf
	happy -i -gcad Gramatyka/ParLatte.y
	alex -g Gramatyka/LexLatte.x

tests:
