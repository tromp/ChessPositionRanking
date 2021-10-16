all:	CountChess randomRs cpr legal

CountChess:	Makefile CountChess.hs
		ghc -O CountChess.hs

randomRs:	Makefile randomRs.hs
		ghc -O --make randomRs.hs -o randomRs

cpr:	Makefile Chess/Position.hs Data/Ranking.hs Chess/Position/Ranking.hs Data/Ranking/Batched.hs Main.hs
	ghc -O --make Main.hs -o cpr

legal:	Makefile Legality.hs
	ghc -O Legality.hs -o legal

sortFENs:	Makefile SortFENs.hs
	ghc -O SortFENs.hs -o sortFENs

clean: 	*.o *.hi
	find . -name "*.o" -type f -delete
	find . -name "*.hi" -type f -delete
