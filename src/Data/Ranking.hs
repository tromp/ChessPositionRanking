module Data.Ranking (Ranking(..), multinomialRanking, finiteRanking, listRanking, sizeRanking, bindR, bindUR, bindRI, bindURI, composeRI, composeURI) where

import Data.Ord
import Data.Array
import Data.Maybe
import Data.List
 
-- rank and unrank size items to/from 0..size-1
data Ranking a = Ranking {
  size   :: Integer,
  cmp    :: a -> a -> Ordering,
  unrank :: Integer -> a,
  rank   :: a -> Integer
}

-- lexicographically rank lists containing given numbers of elements
multinomialRanking :: Eq a => [(a, Int)] -> Ranking [a]
multinomialRanking itemCounts = Ranking size cmp unrank rank where
  cnts = map snd itemCounts 
  total = sum cnts
  size = fac total `div` product (map fac cnts) where
    fac n = fac_!n where
    fac_ = listArray (0,total) (scanl (*) 1 [1..fromIntegral total])
  cmp [] [] = EQ
  cmp (a:as) (b:bs) = if cmpiab == EQ then cmp as bs else cmpiab where
    cmpiab = compare ia ib 
    (Just ia) = elemIndex a items
    (Just ib) = elemIndex b items
    items = map fst itemCounts
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
finiteRanking size = Ranking size compare id id

-- rank itens from list
listRanking :: (Eq a, Show a) => [a] -> Ranking a
listRanking l = Ranking size cmp unrank rank where
  size = fromIntegral $ length l
  cmp a b = compare ia ib where
    (Just ia) = elemIndex a l
    (Just ib) = elemIndex b l
  unrank i = l !! fromIntegral i
  rank x = case elemIndex x l of
    Nothing -> error $ show x ++ " not in " ++ show l
    Just i -> fromIntegral i

-- rank pairs (0,0)..(0,size0-1), (1,0)..(1,size1-1), ... ,(k,0)..(k,sizek-1)
sizeRanking :: Eq a => [(a, Integer)] -> Ranking (a, Integer)
sizeRanking itemSizes = Ranking size cmp unrank rank where
  size = sum . map snd $ itemSizes
  cmp (a0,i0) (a1,i1) = if cmpai == EQ then compare i0 i1 else cmpai where
    cmpai = compare ai0 ai1 
    (Just ai0) = elemIndex a0 items
    (Just ai1) = elemIndex a1 items
    items = map fst itemSizes
  unrank = exhaust itemSizes where
    exhaust ((a,sz):iS) i = if i < sz then (a,i) else exhaust iS (i - sz)
  rank (a,i) = i + sum (map snd (takeWhile ((/= a) . fst) itemSizes))

-- analogue of functor function fmap for Rankings
mapR :: (b -> a) -> (a -> b) -> Ranking a -> Ranking b
mapR inv f ra = Ranking sz cmpr unrnk rnk where
  sz    = size ra
  cmpr b0 b1 = cmp ra (inv b0) (inv b1)
  unrnk = f . unrank ra
  rnk   = rank ra . inv

-- analogue of monadic bind for arbitrarily sized Rankings
bindR :: (b -> a) -> Ranking a -> (a -> Ranking b) -> Ranking b
bindR inv ra arb = Ranking sz cmpr unrnk rnk where
  sizeR   = sizeRanking [(i, size . arb . unrank ra $ i) | i <- [0..size ra-1]]
  sz      = size sizeR
  cmpr b0 b1 = if cmp ra a0 a1 == EQ then cmp (arb a0) b0 b1 else cmp ra a0 a1 where
    (a0, a1) = (inv b0, inv b1)
  unrnk i = uncurry (unrank . arb . unrank ra) (unrank sizeR i)
  rnk b   = let a = inv b in rank sizeR (rank ra a, rank (arb a) b)

-- analogue of monadic bind for Uniformly sizes Rankings
bindUR :: (b -> a) -> Ranking a -> (a -> Ranking b) -> Ranking b
bindUR inv ra arb = Ranking sz cmpr unrnk rnk where
  sz      = sizeRa * sizeRb
  sizeRa  = size ra
  sizeRb  = size (arb (unrank ra 0))
  cmpr b0 b1 = if cmp ra a0 a1 == EQ then cmp (arb a0) b0 b1 else cmp ra a0 a1 where
    (a0, a1) = (inv b0, inv b1)
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
