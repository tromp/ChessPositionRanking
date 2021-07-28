module Data.Ranking (Ranking(..), multinomialRanking, finiteRanking, listRanking, sizeRanking, bindR, bindUR, bindRI, bindURI, composeRI, composeURI) where

import Data.Array
import Data.Maybe
import Data.List
 
-- rank and unrank size items to/from 0..size-1
data Ranking a = Ranking {
  size   :: Integer,
  unrank :: Integer -> a,
  rank   :: a -> Integer
}

-- lexicographically rank lists containing given numbers of elements
multinomialRanking :: Eq a => [(a, Int)] -> Ranking [a]
multinomialRanking itemCounts = Ranking size unrank rank where
  cnts = map snd itemCounts 
  total = sum cnts
  size = fac total `div` product (map fac cnts) where
    fac n = fac_!n where
    fac_ = listArray (0,total) (scanl (*) 1 [1..fromIntegral total])
  unrank = place total itemCounts size where
    place 0 _     _    0 = []
    place 0 _     _    _ = error "nonzero i"
    place n itemCounts size i = setItem id itemCounts i where
      setItem pref (ic@(item,cnt):ics) i = if i < size'
       then item : place (n-1) (pref ((item,cnt-1):ics)) size' i
       else setItem (pref . (ic:)) ics (i - size') where
         size' = size * (fromIntegral cnt) `div` (fromIntegral n)
  rank = unplace total itemCounts size where
    unplace :: Eq a => Int -> [(a,Int)]-> Integer -> [a] -> Integer
    unplace 0 _          _    l    = if null l then 0 else error "non-null list"
    unplace n itemCounts size (item:items)= rnkItem id itemCounts where
      rnkItem pref (ic@(it,cnt):ics) = if it == item
       then unplace (n-1) (pref ((item,cnt-1):ics)) size' items 
       else size' + rnkItem (pref . (ic:)) ics where
         size' = size * (fromIntegral cnt) `div` (fromIntegral n)

-- rank Integers from 0..size-1
finiteRanking :: Integer -> Ranking Integer
finiteRanking size = Ranking size id id

-- rank itens from list
listRanking :: (Eq a, Show a) => [a] -> Ranking a
listRanking l = Ranking size unrank rank where
  size = fromIntegral $ length l
  unrank i = l !! fromIntegral i
  rank x = case elemIndex x l of
    Nothing -> error $ show x ++ " not in " ++ show l
    Just i -> fromIntegral i

-- rank pairs (0,0)..(0,size0-1), (1,0)..(1,size1-1), ... ,(k,0)..(k,sizek-1)
sizeRanking :: Eq a => [(a, Integer)] -> Ranking (a, Integer)
sizeRanking itemSizes = Ranking size unrank rank where
  size = sum . map snd $ itemSizes
  unrank = exhaust itemSizes where
    exhaust ((a,sz):iS) i = if i < sz then (a,i) else exhaust iS (i - sz)
  rank (a,i) = i + sum (map snd (takeWhile ((/= a) . fst) itemSizes))

-- analogue of functor function fmap for Rankings
mapR :: (b -> a) -> (a -> b) -> Ranking a -> Ranking b
mapR inv f ra = Ranking sz unrnk rnk where
  sz    = size ra
  unrnk = f . unrank ra
  rnk   = rank ra . inv

-- analogue of monadic bind for arbitrarily sized Rankings
bindR :: (b -> a) -> Ranking a -> (a -> Ranking b) -> Ranking b
bindR inv ra arb = Ranking sz unrnk rnk where
  sizeR   = sizeRanking [(i, size . arb . unrank ra $ i) | i <- [0..size ra-1]]
  sz      = size sizeR
  unrnk i = uncurry (unrank . arb . unrank ra) (unrank sizeR i)
  rnk b   = let a = inv b in rank sizeR (rank ra a, rank (arb a) b)

-- analogue of monadic bind for Uniformly sizes Rankings
bindUR :: (b -> a) -> Ranking a -> (a -> Ranking b) -> Ranking b
bindUR inv ra arb = Ranking sz unrnk rnk where
  sz      = sizeRa * sizeRb
  sizeRa  = size ra
  sizeRb  = size (arb (unrank ra 0))
  unrnk i = uncurry (unrank . arb . unrank ra) (i `divMod` sizeRb)
  rnk b   = let a = inv b in rank ra a * sizeRb + rank (arb a) b

-- non-uniform ranking bind with identity inverse
bindRI :: Ranking a -> (a -> Ranking a) -> Ranking a
bindRI = bindR id

-- uniform ranking bind with identity inverse
bindURI :: Ranking a -> (a -> Ranking a) -> Ranking a
bindURI = bindUR id

-- non-uniform ranking bind with identity inverse
composeRI :: (a -> Ranking a) -> (a -> Ranking a) -> (a -> Ranking a)
composeRI f g = \x -> f x `bindRI` g
infixr 1 `composeRI`

-- uniform ranking bind with identity inverse
composeURI :: (a -> Ranking a) -> (a -> Ranking a) -> (a -> Ranking a)
composeURI f g = \x -> f x `bindURI` g
infixr 2 `composeURI`
