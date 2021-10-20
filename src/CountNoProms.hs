{-# LANGUAGE TupleSections #-}
module Main where

import Control.Monad
import Data.Array
import qualified Data.Map as M
 
-- tuple of #pieces #pawns #bishops #factorial_product
data Army = Army !Int !Int !Int !Integer deriving (Eq, Ord, Show)

-- given a number of fixed pawns (normally 0, but 1 with fixed en-passant)
-- and a number of fixed rooks (normally 0, but 1 or 2 with castling)
-- return list of possibly armies
armies :: [Army]
armies = do
  q <- [0..1]           -- number of queens
  r <- [0..2]          -- number of rooks
  b <- [0..2]           -- number of bishops
  n <- [0..2]           -- number of knights
  p <- [0..8]         -- number of pawns
  return $ Army (1+q+r+b+n) p b (fac q * fac r * fac b * fac n)
 
-- precompute first 63 factorials into array for efficiency
fac :: Int -> Integer
fac n = fac_!n where
fac_ = listArray (0,64) (scanl (*) 1 [1..64])
 
-- precompute first 65x29 falling powers into array for efficiency
fp :: Int -> Int -> Integer
fp n k = fp_!(n,k)
fp_ = array ((0,0),(64,28)) $ do
  n <- [0..64]
  ((n,0), 1) : [((n,k+1), fp n k * fromIntegral (n-k)) | k<-[0..27]]
  -- let n' = fromIntegral n in zip (zip (repeat n) [0..]) (scanl (*) 1 [n',n'-1..n'-27])
 
-- precompute first 49x9x9 trinomial coefficients into array for efficiency
choose2 :: Int -> Int -> Int -> Integer
choose2 0 0 0 = 1
choose2 n k1 k2 = if k1<0||k2<0||k1+k2>n then 0 else choose2_!(n,k1,k2)
choose2_ = array ((1,0,0),(48,8,8)) [((n,k1,k2), let c = choose2 (n-1) in c (k1-1) k2 + c k1 (k2-1) + c k1 k2) | n<-[1..48],  k1<-[0..min n 8], k2<-[0..min (n-k1) 8]]

-- given the number of white and black rooks fixed by castling
-- and the number of pawns to fix for each color (1 for en passant)
-- return the number of possible diagrams
count :: Integer
count = sum $ do
  let np = 8 -- number of pawns
  Army wpcs wp wb wprod <- armies
  let wpx = np-wp            -- white pawns captured
  Army bpcs bp bb bprod <- armies
  let bpx = np-bp            -- black pawns captured
  -- number of captures
  let caps = 32-wp-bp-wpcs-bpcs
  -- a pawn can pass its original opposite if either captures or latter is captured
  -- guard $ 0 == wproms <= caps + bpx
  -- guard $ 0 == bproms <= caps + wpx
  -- the slack in these inequalities limits unopposed pawn 
  -- as they could promote without increasing captures
  let maxuwp = bpx + caps -- unopposed white pawns
  let maxubp = wpx + caps -- unopposed black pawns
  guard $ maxuwp >= 0
  guard $ maxubp >= 0
  -- white (resp. black) must have fixp+wp-maxuwp (resp. fixp+bp-maxbwp) of its pawns opposed
  -- min #files with opposing pawns (multiple opposing per file considered overcounted)
  let minopp = max 0 (wp-maxuwp)
  let space = 64-wp-bp -- space for pieces
  -- choose wp+bp among pawn space and then all pawns/pieces among space-wp-bp
  let cnt = (pawns 0 wp bp minopp * fp space (wpcs+bpcs) `div` (wprod * bprod))
  let spc = fromIntegral space
  return $ let p = fromIntegral (wp+bp) in case max 0 (wb-1) + max 0 (bb-1) of
    0 -> cnt
    1 -> cnt * (spc+1) `div` (2 * (spc-1)) -- account for one side's bishop-pair
    2 -> cnt * (spc+1) `div` (4 * (spc-3)) -- account for both side's bishop-pair
 
-- ways to distribute wp white pawns and bp black pawns over space ps with opposing pawns on opp files
pawns :: Int -> Int -> Int -> Int -> Integer
pawns _ wp bp opp = sum [fromIntegral (mopps opp s 8) * choose2 (48-2*opp-s) (wp-opp) (bp-opp) | s <- [0..4*opp]]

-- opps p s os counts ways for p opposing pawns to sandwich s others in n files
opps :: Int -> Int -> Int -> Int
opps 0 0 _ = 1  -- done
opps 0 _ _ = 0  -- short of sandwiched space
opps _ _ 0 = 0  -- no space left for pawns
opps p s n = mopps p s (n-1) + sum [(5-i) * mopps (p-1) (s-i) (n-1) | i <- [0..min s 4]]

-- precomputed version
mopps :: Int -> Int -> Int -> Int
mopps p s n = mopps_!(p,s,n) where
mopps_ = array ((0,0,0),(8,32,8)) [((p,s,n), opps p s n) | p<-[0..8], s<-[0..32], n<-[0..8]] where

main = print $ count

{--

$ time src/noproms
2891398339895958031893456691621753206624
        0.36 real         0.06 user         0.00 sys

--}
