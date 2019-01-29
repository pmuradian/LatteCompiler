installandcompile:
	cabal update
	cabal install missingh
	cabal install split
	make compile

compile:
	happy -gca ParLatte.y
	alex -g LexLatte.x
	ghc --make TestLatte.hs -o latc_llvm

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi *.bak

distclean: clean
	-rm -f DocLatte.* LexLatte.* ParLatte.* LayoutLatte.* SkelLatte.* PrintLatte.* TestLatte.* AbsLatte.* TestLatte ErrM.* SharedString.* ComposOp.* latte.dtd XMLLatte.* Makefile*
	
testGood: compile
	./latc_llvm ~/Downloads/lattests/good/core001.lat
	./latc_llvm ~/Downloads/lattests/good/core002.lat
	./latc_llvm ~/Downloads/lattests/good/core003.lat
	./latc_llvm ~/Downloads/lattests/good/core004.lat
	./latc_llvm ~/Downloads/lattests/good/core005.lat
	./latc_llvm ~/Downloads/lattests/good/core006.lat
	./latc_llvm ~/Downloads/lattests/good/core007.lat
	./latc_llvm ~/Downloads/lattests/good/core008.lat
	./latc_llvm ~/Downloads/lattests/good/core009.lat
	./latc_llvm ~/Downloads/lattests/good/core010.lat
	./latc_llvm ~/Downloads/lattests/good/core011.lat
	./latc_llvm ~/Downloads/lattests/good/core012.lat
	./latc_llvm ~/Downloads/lattests/good/core013.lat
	./latc_llvm ~/Downloads/lattests/good/core014.lat
	./latc_llvm ~/Downloads/lattests/good/core015.lat
	./latc_llvm ~/Downloads/lattests/good/core016.lat
	./latc_llvm ~/Downloads/lattests/good/core017.lat
	./latc_llvm ~/Downloads/lattests/good/core018.lat
	./latc_llvm ~/Downloads/lattests/good/core019.lat
	./latc_llvm ~/Downloads/lattests/good/core020.lat
	./latc_llvm ~/Downloads/lattests/good/core021.lat
	./latc_llvm ~/Downloads/lattests/good/core022.lat

testBad: compile
	./latc_llvm ~/Downloads/lattests/bad/bad001.lat
	./latc_llvm ~/Downloads/lattests/bad/bad002.lat
	./latc_llvm ~/Downloads/lattests/bad/bad003.lat
	./latc_llvm ~/Downloads/lattests/bad/bad004.lat
	./latc_llvm ~/Downloads/lattests/bad/bad005.lat
	./latc_llvm ~/Downloads/lattests/bad/bad006.lat
	./latc_llvm ~/Downloads/lattests/bad/bad007.lat
	./latc_llvm ~/Downloads/lattests/bad/bad008.lat
	./latc_llvm ~/Downloads/lattests/bad/bad009.lat
	./latc_llvm ~/Downloads/lattests/bad/bad010.lat
	./latc_llvm ~/Downloads/lattests/bad/bad011.lat
	./latc_llvm ~/Downloads/lattests/bad/bad012.lat
	./latc_llvm ~/Downloads/lattests/bad/bad013.lat
	# ./latc_llvm ~/Downloads/lattests/bad/bad014.lat
	./latc_llvm ~/Downloads/lattests/bad/bad015.lat
	./latc_llvm ~/Downloads/lattests/bad/bad016.lat
	./latc_llvm ~/Downloads/lattests/bad/bad017.lat
	./latc_llvm ~/Downloads/lattests/bad/bad018.lat
	./latc_llvm ~/Downloads/lattests/bad/bad019.lat
	./latc_llvm ~/Downloads/lattests/bad/bad020.lat
	./latc_llvm ~/Downloads/lattests/bad/bad021.lat
	./latc_llvm ~/Downloads/lattests/bad/bad022.lat
	./latc_llvm ~/Downloads/lattests/bad/bad023.lat
	./latc_llvm ~/Downloads/lattests/bad/bad024.lat
	./latc_llvm ~/Downloads/lattests/bad/bad025.lat
	./latc_llvm ~/Downloads/lattests/bad/bad026.lat
	./latc_llvm ~/Downloads/lattests/bad/bad027.lat
