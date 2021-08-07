module Main where

import System.Environment
import System.Random
import Data.List

rands :: Integer -> Int -> [Integer]
rands range seed = unfoldr (Just . randomR (0,range-1)) $ mkStdGen seed

main = do
  args <- getArgs
  case args of
    (rstr:nstr:rest) -> mapM_ print . take n . rands range $ seed where
      n     = read nstr :: Int
      range = read rstr :: Integer
      seed  = case rest of
        [sdstr] -> read sdstr :: Int
        []      -> 0
    _ -> putStrLn "usage: randomRs <range> <n> [<seed>]"
