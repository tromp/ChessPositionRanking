module SortFENs where

import System.Environment
import Data.Ranking.Batched
import Data.List
import Chess.Position
import Chess.Position.Ranking

-- Chess Position Ranking (Batched)
cpr :: BRanking URPosition
cpr = batch sideToMoveRanking `composeURI` (batch caseRanking `composeRI` batch wArmyStatRanking `composeURI` batch bArmyStatRanking `composeRI` batch guardRanking `composeRI` batch enPassantRanking `composeURI` batch epOppRanking `composeURI` batch sandwichRanking `composeRI` batch opposeRanking `composeURI` batch pawnRanking `composeURI` batch castleRanking `composeURI` batch wArmyRanking `composeURI` batch bArmyRanking `composeURI` batch pieceRanking) $ emptyURPosition

-- main is mostly a command line version of sortBy (cmp cpr)
main = do
  let processLines f = getContents >>= mapM_ putStrLn . f . filter (not . null) . lines
  processLines $ map (writeFEN . toPosition). sortBy (cmp cpr) . map (fromPosition . readFEN)
