all:	CountChess randomRs cpr legal sortFENs

CountChess:	Makefile CountChess.hs
		ghc -O CountChess.hs

randomFENs:	Makefile randomFENs.hs
		ghc -O --make RandomFENs.hs -o randomFENs

randomRs:	Makefile RandomRs.hs
		ghc -O --make RandomRs.hs -o randomRs

cpr:	Makefile Chess/Position.hs Data/Ranking.hs Chess/Position/Ranking.hs Data/Ranking/Batched.hs Main.hs
	ghc -O --make Main.hs -o cpr

legal:	Makefile Legality.hs
	ghc -O Legality.hs -o legal

sortFENs:	Makefile SortFENs.hs
	ghc -O SortFENs.hs -o sortFENs

clean: 	*.o *.hi
	find . -name "*.o" -type f -delete
	find . -name "*.hi" -type f -delete
