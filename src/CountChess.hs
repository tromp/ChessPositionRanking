{-# LANGUAGE TupleSections #-}
module CountChess where

import Control.Monad
import Data.Array
import qualified Data.Map as M
 
-- tuple of #pieces #pawns #promotions #factorial_product
data Army = Army !Int !Int !Int !Integer deriving (Eq, Ord, Show)

-- given a number of fixed pawns (normally 0, but 1 with fixed en-passant)
-- and a number of fixed rooks (normally 0, but 1 or 2 with castling)
-- return list of possibly armies
_armies :: Int -> Int -> [Army]
_armies fixr fixp = do
  let ur = 2 - fixr             -- number of unfixed rooks
  k <- [ur `div `2]             -- number of kings
  let np0 = 8 - fixp            -- number of promotable pawns
  q <- [0..1+np0]               -- number of queens
  let np1 = np0 - max (q-1) 0   -- adjust for queen promotions
  r <- [0..ur+np1]              -- number of rooks
  let np2 = np1 - max (r-ur) 0  -- adjust for rook promotions
  b <- [0..2+np2]               -- number of bishops
  let np3 = np2 - max (b-2) 0   -- adjust for bishop promotions
  n <- [0..2+np3]               -- number of knights
  let np4 = np3 - max (n-2) 0   -- adjust for knight promotions
  let proms = np0 - np4         -- number of promotions
  p <- [0..np4]                 -- number of pawns
  return $ Army (k+q+r+b+n) p proms (fac k * fac q * fac r * fac b * fac n)
 
-- pair unique elements in a list with their multiplicity
count_unique :: Ord a => [a] -> [(a ,Integer)]
count_unique = M.toList . M.fromListWith (+) . map (,1)

-- precompute unique armies with multiplicity into array
-- indexd by 3x2 parameter combinations for efficiency
armies :: Int -> Int -> [(Army, Integer)]
armies fixr fixp = armies_!(fixr,fixp)
armies_ = array ((0,0),(2,1)) [((fr,fp), count_unique (_armies fr fp)) | fr<-[0..2], fp<-[0..1]]
 
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
count :: Int -> Int -> Int -> Integer
count fixwr fixbr fixp = sum $ do
  let np = 8 - fixp                 -- number of unfixed pawns
  let fixwk = if fixwr /= 0 then 1 else 0
  (Army wpcs wp wproms wprod, wmul) <- armies fixwr fixp
  let wpx = np-wp-wproms            -- white pawns captured
  let fixbk = if fixbr /= 0 then 1 else 0
  (Army bpcs bp bproms bprod, bmul) <- armies fixbr fixp
  let bpx = np-bp-bproms            -- black pawns captured
  -- number of captures
  let caps = 32-2*fixp-fixwk-fixbk-fixwr-fixbr-wp-bp-wpcs-bpcs
  -- a pawn can pass its original opposite if either captures or latter is captured
  -- guard $ wproms <= caps + bpx
  -- guard $ bproms <= caps + wpx
  -- the slack in these inequalities limits unopposed pawn 
  -- as they could promote without increasing captures
  let maxuwp = bpx + caps - wproms  -- unopposed white pawns
  let maxubp = wpx + caps - bproms  -- unopposed black pawns
  guard $ maxuwp >= 0
  guard $ maxubp >= 0
  -- white (resp. black) must have fixp+wp-maxuwp (resp. fixp+bp-maxbwp) of its pawns opposed
  -- min #files with opposing pawns (multiple opposing per file considered overcounted)
  let minopp = max 0 (fixp + wp-maxuwp)
  let space = 64-4*fixp-fixwk-fixbk-fixwr-fixbr-wp-bp -- space for pieces
  -- choose wp+bp among pawn space and then all pawns/pieces among space-wp-bp
  return $ wmul * bmul * (pawns fixp wp bp minopp * fp space (wpcs+bpcs) `div` (wprod * bprod))
 
-- ways to distribute wp white pawns and bp black pawns over space ps with opposing pawns on opp files
pawns :: Int -> Int -> Int -> Int -> Integer
pawns 0 wp bp opp = sum [fromIntegral (mopps opp s 8) * choose2 (48-2*opp-s) (wp-opp) (bp-opp) | s <- [0..4*opp]]
pawns 1 wp bp opp = pawnsep 0 0 0
             + sum [pawnsep 1 1 (ds1+ds2) | opp>1, ds1 <- [0..2], ds2 <- [0..1]]
             + sum [pawnsep 1 0  ds1      | opp>0, ds1 <- [0..2]]
             + sum [pawnsep 0 1      ds2  | opp>0, ds2 <- [0..1]] where
  -- put dw white pawns in file of black pawn just moved
  -- and db black pawns opposite white's pawn that can capture it en-passant
  -- together spanning ds sandwiched space
  pawnsep dw db ds = let opp' = opp-dw-db in sum [fromIntegral (mopps opp' s 6) * choose2 (44-opp-opp'-ds-s) (wp+db-opp) (bp+dw-opp) | s <- [0..4*opp']]

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

cases :: [(Int, Int, Int)]
cases = [(fwr,fbr,ep) | fwr <- [0..2], fbr <- [0..2], ep <- [0..1]]

multFR :: Int -> Integer
multFR 0 = 1
multFR 1 = 2
multFR 2 = 1

multEP :: Int -> Integer
multEP 0 = 1
-- each of the squares a5-h5 can have a black pawn en-passant
-- capturable by 2 white pawns, except a5/h5, which could only
-- be captured by 1 white pawn, giving 8*2-2 = 14 multiplier
multEP 1 = 14

main = let
  -- given fixed white and fixed black rooks,
  -- en passant flag
  -- per-file pre-occupied central squares
  -- a multiplier
  -- show and return number of possible positions
  -- this assumes white-to-move
  showcount :: (Int, Int, Int) -> IO Integer
  showcount (fwr,fbr,ep) = do
    let mul = multFR fwr * multFR fbr * multEP ep
    let cnt = count fwr fbr ep * mul
    putStrLn $ "fixwr=" ++ show fwr ++ " fixbr=" ++ show fbr ++ " ep=" ++ show ep ++ " " ++ show cnt
    return cnt
  in do
  whiteToMove <- sum <$> mapM showcount cases
  putStr "total positions: "
  -- adjust for either side-to-move
  print $ 2 * whiteToMove

{--

$ time ./CountChess
fixwr=0 fixbr=0 ep=0 4317116501858047620299900728599356147494556640
fixwr=0 fixbr=0 ep=1 31999595200733582973106880061728861929069928
fixwr=0 fixbr=1 ep=0 6922142764395483618137561107749568789790148
fixwr=0 fixbr=1 ep=1 54444384271688044810990508417111948991768
fixwr=0 fixbr=2 ep=0 136530769984693307040227830128737122354029
fixwr=0 fixbr=2 ep=1 1035365889143551932685537903363800096422
fixwr=1 fixbr=0 ep=0 6922142764395483618137561107749568789790148
fixwr=1 fixbr=0 ep=1 54308353407259673025385006313129004055128
fixwr=1 fixbr=1 ep=0 11745419798256512510493235052589222172668
fixwr=1 fixbr=1 ep=1 98172517157950055940864091510815802248
fixwr=1 fixbr=2 ep=0 235958281122206691085936171885340863152
fixwr=1 fixbr=2 ep=1 1903336650826558863672909067902430108
fixwr=2 fixbr=0 ep=0 136530769984693307040227830128737122354029
fixwr=2 fixbr=0 ep=1 1028637537652354045548219736317757984742
fixwr=2 fixbr=1 ep=0 235958281122206691085936171885340863152
fixwr=2 fixbr=1 ep=1 1895642836539897140574420164647093916
fixwr=2 fixbr=2 ep=0 4729971278292293446735355275667009679
fixwr=2 fixbr=2 ep=1 36635290891989131864827262732080222
total positions: 8726713169886222032347729969256422370854716254

real	0m8.300s
user	0m8.228s
sys	0m0.043s

--}
