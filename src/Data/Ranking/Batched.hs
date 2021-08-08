module Data.Ranking.Batched (BRanking(..), batch, sizeRanking, bindR, bindUR, bindRI, bindURI, composeRI, composeURI) where

import Data.Array
import Data.Function
import Data.List
import qualified Data.Ranking as R
 
-- rank and unrank size items to/from 0..size-1
data BRanking a = BRanking {
  size   :: Integer,
  unrank :: [Integer] -> [a],
  rank   :: [a] -> [Integer]
}

-- (trivially) turn a function returning a ranking into one returning a batched ranking
batch :: (a -> R.Ranking b) -> a -> BRanking b
batch arb a = let r = arb a in BRanking {
  size   = R.size r,
  unrank = map (R.unrank r),
  rank   = map (R.rank r)
}

-- rank pairs (0,0)..(0,size0-1), (1,0)..(1,size1-1), ... ,(k,0)..(k,sizek-1)
sizeRanking :: (Show a, Eq a) => [(a, Integer)] -> BRanking (a, Integer)
sizeRanking itemSizes = BRanking size unrank rank where
  size = sum . map snd $ itemSizes
  unrank = urnk 0 itemSizes where
    urnk _ _ [] = []
    urnk sum iS@((a,sz):iS') is@(i:is') = if i-sum < sz then (a,i-sum):urnk sum iS is' else urnk (sum+sz) iS' is
  rank = rnk 0 itemSizes where
    rnk _ _ [] = []
    rnk sum [] ais@((a,i):ais') = error $ "nothing left to rank " ++ show (a,i)
    rnk sum iS@((a',sz):iS') ais@((a,i):ais') = if a==a' then (sum+i):rnk sum iS ais' else rnk (sum+sz) iS' ais

-- group snd's of pairs with same fst
groupByFst :: (Eq a) => [(a,b)] -> [(a,[b])]
groupByFst = map (\xs@((x,_):_) -> (x, map snd xs)). groupBy ((==) `on` fst)

-- test if list has nondecreasing values
nondecreasing :: (Ord a) => [a] -> Bool
nondecreasing l = and [x <= y  | (x:y:_) <- tails l]

-- analogue of monadic bind for arbitrarily sized Rankings
bindR :: (b -> a) -> BRanking a -> (a -> BRanking b) -> BRanking b
bindR inv ra arb = BRanking sz unrnk rnk where
  aranks   = [0.. size ra-1]
  sizeR    = sizeRanking $ zip aranks (map (size . arb) . unrank ra $ aranks)
  sz       = size sizeR
  unrnk is = concat . zipWith unrank rbs . map snd $ aibis where
    rbs   =  map arb . unrank ra . map fst $ aibis
    aibis = groupByFst . unrank sizeR $ is
  rnk bs   = rank sizeR [(ai,bi) | (ai, bs) <- aibs, let rb = arb . inv . head $ bs, bi <- rank rb bs] where
    ais = rank ra . map inv $ bs
    -- aibs = if nondecreasing ais then groupByFst (zip ais bs) else error ("decreasing " ++ show ais)
    aibs = groupByFst (zip ais bs)

-- analogue of monadic bind for Uniformly sized Rankings
bindUR :: (b -> a) -> BRanking a -> (a -> BRanking b) -> BRanking b
bindUR inv ra arb = BRanking sz unrnk rnk where
  sz       = sizeRa * sizeRb
  sizeRa   = size ra
  sizeRb   = size . arb . head . unrank ra $ [0]
  unrnk is = concat . zipWith unrank rbs . map snd $ aibis where
    rbs   =  map arb . unrank ra . map fst $ aibis
    aibis = groupByFst . map (`divMod` sizeRb) $ is
  rnk bs   = [(ai*sizeRb+bi) | (ai, bs) <- aibs, let rb = arb . inv . head $ bs, bi <- rank rb bs] where
    ais = rank ra . map inv $ bs
    -- aibs = if nondecreasing ais then groupByFst (zip ais bs) else error ("decreasing " ++ show ais)
    aibs = groupByFst (zip ais bs)

-- non-uniform ranking bind with identity inverse
bindRI :: BRanking a -> (a -> BRanking a) -> BRanking a
bindRI = bindR id

-- uniform ranking bind with identity inverse
bindURI :: BRanking a -> (a -> BRanking a) -> BRanking a
bindURI = bindUR id

-- non-uniform ranking bind with identity inverse
composeRI :: (a -> BRanking a) -> (a -> BRanking a) -> (a -> BRanking a)
composeRI f g = \x -> f x `bindRI` g
infixr 1 `composeRI`

-- uniform ranking bind with identity inverse
composeURI :: (a -> BRanking a) -> (a -> BRanking a) -> (a -> BRanking a)
composeURI f g = \x -> f x `bindURI` g
infixr 2 `composeURI`
