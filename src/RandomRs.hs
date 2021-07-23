import System.Environment
import System.Random
import Data.List

rands :: Integer -> [Integer]
rands range = unfoldr (Just . randomR (0,range-1)) $ mkStdGen 0

main = do
  args <- getArgs
  case args of
    [nstr, rstr] -> mapM_ print . take n . rands $ range where
       n     = read nstr :: Int
       range = read rstr :: Integer
    _ -> putStrLn "usage: randomRs <n> <range>"
