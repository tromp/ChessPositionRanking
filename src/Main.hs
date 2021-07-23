module Main where

import System.Environment
import Chess.Position
import Data.Ranking
import Chess.Position.Ranking

-- main is mostly a command line batched version of cpr
main = do
  args <- getArgs
  let processLines f = getContents >>= mapM_ (putStrLn . f) . filter (not . null) . lines
  case args of
    ["size"]   -> print $ size cpr
    ["rank"]   -> processLines $ unwords . map (show . rank cpr) . makeUR . readFEN
    ["unrank"] -> processLines $ unique . map (writeFEN . toPosition . unrank cpr . read) . words where
       unique (x:xs) = if all (==x) xs then x else error "different positions"
    _ -> putStrLn "usage: cpr [size|rank|unrank]"
