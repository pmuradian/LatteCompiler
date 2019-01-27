all:
	happy -gca ParLatte.y
	alex -g LexLatte.x
	ghc --make TestLatte.hs -o TestLatte

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi *.bak

distclean: clean
	-rm -f DocLatte.* LexLatte.* ParLatte.* LayoutLatte.* SkelLatte.* PrintLatte.* TestLatte.* AbsLatte.* TestLatte ErrM.* SharedString.* ComposOp.* latte.dtd XMLLatte.* Makefile*
	
test: all
	./TestLatte ~/Downloads/lattests/bad/bad013.lat
	# ./TestLatte ~/Downloads/lattests/good/core001.lat
	# ./TestLatte ~/Downloads/lattests/good/core003.lat
	# ./TestLatte ~/Downloads/lattests/good/core009.lat
	# ./TestLatte ~/Downloads/lattests/good/core0021.lat
	# ./TestLatte ~/Downloads/lattests/good/core0017.lat