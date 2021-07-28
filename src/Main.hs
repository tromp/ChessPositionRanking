module Main where

import System.Environment
import Data.Ranking.Batched
import Chess.Position
import Chess.Position.Ranking

-- Chess Position Ranking (Batched)
cpr :: BRanking URPosition
cpr = batch sideToMoveRanking `composeURI` (batch caseRanking `composeRI` batch wArmyStatRanking `composeURI` batch bArmyStatRanking `composeRI` batch guardRanking `composeRI` batch enPassantRanking `composeURI` batch epOppRanking `composeURI` batch sandwichRanking `composeRI` batch opposeRanking `composeURI` batch pawnRanking `composeURI` batch castleRanking `composeURI` batch wArmyRanking `composeURI` batch bArmyRanking `composeURI` batch pieceRanking) $ emptyURPosition

-- main is mostly a command line version of cpr
main = do
  args <- getArgs
  let processLines f = getContents >>= mapM_ putStrLn . f . filter (not . null) . lines
  case args of
    ["size"]   -> print $ size cpr
    ["rank"]   -> processLines $ map show . rank cpr . map (fromPosition . readFEN)
    ["unrank"] -> processLines $ map (writeFEN . toPosition) . unrank cpr . map read where
    _ -> putStrLn "usage: cpr [size|rank|unrank]"
