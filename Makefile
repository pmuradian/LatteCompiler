installandcompile:
	cabal update
	cabal install missingh
	cabal install split
	make compile

compile:
	happy -gca ./src/ParLatte.y
	alex -g ./src/LexLatte.x
	ghc --make -isrc ./src/TestLatte.hs -o latc_llvm

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi *.bak

distclean: clean
	-rm -f src/DocLatte.* src/LexLatte.* src/ParLatte.* src/LayoutLatte.* src/SkelLatte.* src/PrintLatte.* src/TestLatte.* src/AbsLatte.* src/TestLatte src/ErrM.* src/SharedString.* src/ComposOp.* src/latte.dtd src/XMLLatte.* Makefile*
	
testGood: compile
	./latc_llvm ./tests/good/core001.lat
	./latc_llvm ./tests/good/core002.lat
	./latc_llvm ./tests/good/core003.lat
	./latc_llvm ./tests/good/core004.lat
	./latc_llvm ./tests/good/core005.lat
	./latc_llvm ./tests/good/core006.lat
	./latc_llvm ./tests/good/core007.lat
	./latc_llvm ./tests/good/core008.lat
	./latc_llvm ./tests/good/core009.lat
	./latc_llvm ./tests/good/core010.lat
	./latc_llvm ./tests/good/core011.lat
	./latc_llvm ./tests/good/core012.lat
	./latc_llvm ./tests/good/core013.lat
	./latc_llvm ./tests/good/core014.lat
	./latc_llvm ./tests/good/core015.lat
	./latc_llvm ./tests/good/core016.lat
	./latc_llvm ./tests/good/core017.lat
	./latc_llvm ./tests/good/core018.lat
	./latc_llvm ./tests/good/core019.lat
	./latc_llvm ./tests/good/core020.lat
	./latc_llvm ./tests/good/core021.lat
	./latc_llvm ./tests/good/core022.lat

testBad: compile
	./latc_llvm ./tests/bad/bad001.lat
	./latc_llvm ./tests/bad/bad002.lat
	./latc_llvm ./tests/bad/bad003.lat
	./latc_llvm ./tests/bad/bad004.lat
	./latc_llvm ./tests/bad/bad005.lat
	./latc_llvm ./tests/bad/bad006.lat
	./latc_llvm ./tests/bad/bad007.lat
	./latc_llvm ./tests/bad/bad008.lat
	./latc_llvm ./tests/bad/bad009.lat
	./latc_llvm ./tests/bad/bad010.lat
	./latc_llvm ./tests/bad/bad011.lat
	./latc_llvm ./tests/bad/bad012.lat
	./latc_llvm ./tests/bad/bad013.lat
	./latc_llvm ./tests/bad/bad015.lat
	./latc_llvm ./tests/bad/bad016.lat
	./latc_llvm ./tests/bad/bad017.lat
	./latc_llvm ./tests/bad/bad018.lat
	./latc_llvm ./tests/bad/bad019.lat
	./latc_llvm ./tests/bad/bad020.lat
	./latc_llvm ./tests/bad/bad021.lat
	./latc_llvm ./tests/bad/bad022.lat
	./latc_llvm ./tests/bad/bad023.lat
	./latc_llvm ./tests/bad/bad024.lat
	./latc_llvm ./tests/bad/bad025.lat
	./latc_llvm ./tests/bad/bad026.lat
	./latc_llvm ./tests/bad/bad027.lat