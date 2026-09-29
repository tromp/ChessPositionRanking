module Main where

import Chess.Position (readFEN, writeFEN)
import Chess.Position.Ranking (toPosition, fromPosition)

fen927 :: String
fen927 = "rnbqkbnr/1p2pppp/p7/2Pp4/3p4/8/PP1PPPPP/RNBQKB1R w KQkq d6 0 5"

fen928 :: String
fen928 = "rnb1kbnr/pp2pppp/8/2Pp4/q2p4/8/PP1PPPPP/RNB1KB1R w KQkq d6 0 6"

expected927 :: String
expected927 = "rnbqkbnr/1p2pppp/p7/2Pp4/3p4/8/PP1PPPPP/RNBQKB1R w KQkq d6 0 1"

expected928 :: String
expected928 = "rnb1kbnr/pp2pppp/8/2Pp4/q2p4/8/PP1PPPPP/RNB1KB1R w KQkq d6 0 1"

assertEqual :: String -> String -> String -> IO ()
assertEqual label expected actual
  | expected == actual = pure ()
  | otherwise = error (label ++ " failed\nexpected: " ++ expected ++ "\nactual:   " ++ actual)

main :: IO ()
main = do
  let roundtrip927 = writeFEN (toPosition (fromPosition (readFEN fen927)))
  assertEqual "#927 reachable en-passant coverage" expected927 roundtrip927

  let roundtrip928 = writeFEN (toPosition (fromPosition (readFEN fen928)))
  assertEqual "#928 multiplicity correction" expected928 roundtrip928

  putStrLn "en-passant regressions: PASS"
