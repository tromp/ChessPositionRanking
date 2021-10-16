module Chess.Position.Ranking (URPosition, fromPosition, toPosition, sideToMoveRanking, caseRanking, wArmyStatRanking, bArmyStatRanking, guardRanking, enPassantRanking, epOppRanking, sandwichRanking, opposeRanking, pawnRanking, castleRanking, wArmyRanking, bArmyRanking, pieceRanking, emptyURPosition) where

import Data.Function
import Data.Maybe
import Data.Char
import Data.List
import Data.List.Split
import Data.Array
import Data.Bits
import qualified Data.Map as M
import Debug.Trace
import Data.Ranking
import Chess.Position
 
pawnFreeSymbol = ','
enPassantSymbol = ':'
whitePieceSymbols = "KQRBN"
blackPieceSymbols = map toLower whitePieceSymbols 
pieceSymbols = whitePieceSymbols ++ blackPieceSymbols

-- #pawns #promotions [#kings,#queens,#rooks,#bishops,#knights]
data Army = Army !Int !Int [Int] deriving (Eq, Ord, Show)

-- army stats; tuple of #total #pawns #promotions #factorial_product
data ArmyStat = ArmyStat !Int !Int !Int !Int deriving (Eq, Ord, Show)

-- a Uniquely Rankable Position
data URPosition = URPosition {
  whiteToMove :: Bool,    -- set in sideToMoveRanking 
  fixwr :: Int, fixbr :: Int, -- set in caseRanking
  fixp :: Int,            -- set in caseRanking
  wArmyStat :: ArmyStat,  -- set in wArmyStatRanking 
  bArmyStat :: ArmyStat,  -- set in bArmyStatRanking 
  minOpp :: Int,          -- set in guardRanking 
  wEP :: Square, bEP :: Square, -- set in enPassantRanking 
  wEPOpp :: Int, bEPOpp :: Int, -- set in epOppRanking
  oppSep :: Int,          -- set in sandwichRanking
  oppList :: [Opposing],  -- set in opposeRanking
  pawnString :: [Symbol], -- set in pawnRanking
  castleMap :: CastleMap, -- set in castleRanking 
  pieceCnts :: [Int],     -- set in wArmyRanking & bArmyRanking  
  pieceString :: [Symbol] -- set in pieceRanking
} deriving (Eq, Ord, Show)

-- empry ArmyStat
emptyStat = ArmyStat 1 0 0 1

-- empty position
emptyURPosition :: URPosition
emptyURPosition = URPosition {
  whiteToMove = True,
  fixwr = 0, fixbr = 0, fixp = 0,
  castleMap = 0,
  wArmyStat = emptyStat,
  bArmyStat = emptyStat,
  minOpp = 0,
  pieceCnts = [0,0,0,0,0,0,0,0,0,0], 
  wEP = 0, bEP = 0,
  wEPOpp = 0, bEPOpp = 0,
  oppSep = 0,
  oppList = [],
  pawnString = "",
  pieceString = ""
}

-- convert to Position
toPosition :: URPosition -> Position
toPosition pos = Position { diagram = diag, sideToMove = stm, castlings = cM, enPassant = ep, num0 = n0, num1 = n1 } where
  URPosition { fixp = fp, whiteToMove = wtm, wEP = wep, bEP = bep, castleMap = cM, oppList = oL, pieceString = pcString } = pos
  stm = if wtm then 'w' else 'b'
  ep = if fp==0 then 0 else if wtm then bep-8 else wep+8
  pawnDiag = pawnDiagram pos 
  pcSquares = filter (\c -> pawnDiag!c == emptySymbol || pawnDiag!c == pawnFreeSymbol) boardSquares
  diag = pawnDiag // zip pcSquares pcString // eraseEP
  eraseEP = if fp == 0 then [] else [(ep, emptySymbol), (if wtm then ep-8 else ep+8, emptySymbol)]
  urR = urChoices pos diag
  n0 = fromIntegral . rank urR $ (wep, bep, oL)
  n1 = fromIntegral $ size urR

-- make opplist of given size
mkOppLists :: Int -> [[Opposing]] -> [[Opposing]]
mkOppLists 0 _ = [[]] -- done picking
mkOppLists mo [] = []  -- too late to pick opps
mkOppLists mo (f:fs) = mkOppLists mo fs ++ [ opp:opps | opp <- f, opps <- mkOppLists (mo-1) fs]

-- create ranking of en-passant and oppose list choices
urChoices :: URPosition -> Diagram -> Ranking (Square, Square, [Opposing])
urChoices (URPosition { whiteToMove = wtm, wEP = wep, bEP = bep, minOpp = minopp }) diag = listRanking choices where
  pep = if wtm then bep else wep
  epfile = pep `mod` 8
  eps = if wtm then [(pep-1, pep) | epfile > 0, diag!(pep-1)=='P']
                 ++ [(pep+1, pep) | epfile < 7, diag!(pep+1)=='P']
               else [(pep, pep-1) | epfile > 0, diag!(pep-1)=='p']
                 ++ [(pep, pep+1) | epfile < 7, diag!(pep+1)=='p']
  choices = [(wep, bep, oL) | (wep, bep) <- if pep == 0 then [(0,0)] else eps, oL <- mkOppLists (max 0 minopp) . mkOpps $ diag]

-- convert from Position
fromPosition :: Position -> URPosition
fromPosition (Position { diagram = diag, sideToMove = stm, castlings = cM, enPassant = ep, num0 = n0, num1 = n1 }) = let
  wtm = stm == 'w'
  wfr = popCount (cM `shiftR` 2 .&. 3)
  bfr = popCount (cM            .&. 3)
  wfk = if wfr==0 then 0 else 1
  bfk = if bfr==0 then 0 else 1
  fxp = if ep==0 then 0 else 1
  allSymbols = "Pp" ++ pieceSymbols
  fixcnts = zip allSymbols [-fxp,-fxp,-wfk,0,-wfr,0,0,-bfk,0,-bfr,0,0]
  cnts = M.fromListWith (+) $ fixcnts ++ zip (elems diag) (repeat 1)
  (wp:bp:pccnts) = map (cnts M.!) allSymbols
  [_,wq,wr,wb,wn,_,bq,br,bb,bn] = pccnts
  wproms = sum . map (max 0) $ [wq-1,wr-(2-wfr),wb-2,wn-2]
  bproms = sum . map (max 0) $ [bq-1,br-(2-bfr),bb-2,bn-2]
  wtot = fxp + wfr + wp + 1 + wq + wr + wb + wn
  btot = fxp + bfr + bp + 1 + bq + br + bb + bn
  maxuwp = let caps = 32 - wtot - btot in 8-fxp - bp - bproms + caps - wproms
  minopp = fxp + wp - maxuwp
  pep = if ep==0 then 0 else if wtm then ep+8 else ep-8
  pos = URPosition {
    whiteToMove = wtm, fixp = fxp,
    fixwr = wfr, fixbr = bfr, castleMap = cM,
    wArmyStat = ArmyStat wtot wp wproms (facProd [wq,wr,wb,wn]),
    bArmyStat = ArmyStat btot bp bproms (facProd [bq,br,bb,bn]),
    minOpp = minopp, pieceCnts = pccnts,
    wEP = pep, bEP = pep,             -- only partially correct
    wEPOpp = 0, bEPOpp = 0, oppSep = 0,              -- not set
    oppList = [], pawnString = "", pieceString = ""  -- not set
  }
  urR = urChoices pos diag
  rnk = if n0 < n1 && n1 == fromIntegral (size urR) then fromIntegral n0 else 0
  (wep, bep, opplist) = unrank urR rnk
  weps = filter ((== wep) . snd) opplist
  beps = filter ((== bep) . fst) opplist
  pos' = pos {
    wEP = wep, bEP = bep, oppList = opplist,
    wEPOpp = length weps, bEPOpp = length beps,
    oppSep = sum [(w-8 - b) `div` 8 | (b,w) <- opplist]
  }
  oppDiag = oppDiagram pos'
  pwnSquares = filter (\c -> oppDiag!c == emptySymbol) pawnSquares
  haspawn c = let sq = diag!c in if toLower sq == 'p' then sq else emptySymbol
  fixDiag = fixDiagram pos'
  pcSquares = filter (\c -> fixDiag!c == emptySymbol && toLower (diag!c) /= 'p') boardSquares
 in pos' { pawnString = map haspawn pwnSquares, pieceString = map (diag!) pcSquares }

-- diagram with only fixed (castling and en-passant) placements
fixDiagram :: URPosition -> Diagram
fixDiagram pos = emptyDiagram // kingAssocs // rookAssocs // epAssocs where
  URPosition { whiteToMove = wtm, fixp = ep, castleMap = cM, wEP = wep, bEP = bep } = pos
  kingAssocs = filterMap cM [(4,'k'),(4,'k'),(60,'K'),(60,'K')]
  rookAssocs = filterMap cM [(0,'r'),(7,'r'),(56,'R'),(63,'R')]
  epAssocs   = if ep == 0 then [] else [(wep,'P'),(bep,'p'),(c,enPassantSymbol),(c+8,enPassantSymbol)] where
    c = if wtm then bep - 16 else wep + 8

-- with additional comma separated opposing pawns
oppDiagram :: URPosition -> Diagram
oppDiagram pos@(URPosition { oppList = oL }) = foldl opp (fixDiagram pos) oL where
  opp diag (b,w) = diag // ((b,'p'):(w,'P'):[(i,pawnFreeSymbol) | i <- [b+8,b+16..w-8]])

-- with additional free pawn placements
pawnDiagram :: URPosition -> Diagram
pawnDiagram pos@(URPosition { pawnString = pS }) = oppDiag // zip pwnSquares pS where
  oppDiag = oppDiagram pos 
  pwnSquares = filter (\c -> oppDiag!c == emptySymbol) pawnSquares

-- show position as string
-- instance Show URPosition where show = showDiagram . diagram
sp = putStrLn . showDiagram . diagram

-- rank side to move
sideToMoveRanking :: URPosition -> Ranking URPosition
sideToMoveRanking pos = Ranking size cmp unrank rank where
  size = 2
  cmp = compare `on` rank
  unrank i = pos { whiteToMove = i == 0 }
  rank (URPosition { whiteToMove = wtm }) = if wtm then 0 else 1

-- rank cases related to castling and en-passant
caseRanking :: URPosition -> Ranking URPosition
caseRanking pos = Ranking size cmp unrank rank where
  -- cases = [(fwr,fbr,ep) | fwr <- [0..2], fbr <- [0..2], ep <- [0..1]]
  size = 3 * 3 * 2
  cmp = compare `on` rank
  unrank i' = pos { fixwr = i `div` 6, fixbr = i `div` 2 `mod` 3, fixp = i `mod` 2 } where i = fromIntegral i'
  rank (URPosition { fixwr = fwr, fixbr = fbr, fixp = ep }) = fromIntegral $ 6 * fwr + 2 * fbr + ep

-- value that's unique for piece count factorial product
facProd :: [Int] -> Int
facProd xs = sum . map (fakeFac!) $ xs where
  -- encode powers of 2, 3, 5, and 7 in bitfields of size 4, 3, 2, and 1
  -- e.g. 9 = 3^2 is represented by a 2 in the bitfield that is 2nd least sign octal 0o020
  fakeFac = listArray (0,10) $ scanl (+) 0 [0,0o100,0o010,0o200,0o002,0o110,0o001,0o300,0o020,0o102]

-- given a number of fixed pawns (normally 0, but 1 with fixed en-passant)
-- and a number of fixed rooks (normally 0, but 1 or 2 with castling)
-- return list of possibly armies
armies :: Int -> Int -> [Army]
armies fixr fixp = do
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
  return $ Army p proms [k,q,r,b,n]

-- precompute unique armyStats with multiplicity into array
-- indexed by 3x2 parameter combinations for efficiency
armyStats :: Int -> Int -> M.Map ArmyStat [[Int]]
armyStats fixr fixp = armyStats_!(fixr,fixp)
armyStats_ = array ((0,0),(2,1)) [((fr,fp), collectedArmies fr fp) | fr<-[0..2], fp<-[0..1]] where
  -- group elements in list paired with common key
  collectedArmies fr fp = M.fromListWith (++) . map (\army@(Army _ _ pcs) -> (armyStat army, [pcs])) $ armies fr fp where
    armyStat (Army p proms pcs) = ArmyStat (fr+fp + p + 1 + sum (tail pcs)) p proms (facProd pcs)

-- rank stats of white army
wArmyStatRanking :: URPosition -> Ranking URPosition
wArmyStatRanking pos@(URPosition { fixwr = fwr, fixp = ep }) = Ranking size cmp unrank rank where
  wArmyStats = armyStats fwr ep
  size = fromIntegral $ M.size wArmyStats
  cmp = compare `on` rank
  unrank i = pos { wArmyStat = fst $ M.elemAt (fromIntegral i) wArmyStats } 
  rank (URPosition { wArmyStat = wAS }) = fromIntegral . M.findIndex wAS $ wArmyStats

-- rank stats of black army
bArmyStatRanking :: URPosition -> Ranking URPosition
bArmyStatRanking pos@(URPosition { fixbr = fbr, fixp = ep }) = Ranking size cmp unrank rank where
  bArmyStats = armyStats fbr ep
  size = fromIntegral $ M.size bArmyStats
  cmp = compare `on` rank
  unrank i = pos { bArmyStat = fst $ M.elemAt (fromIntegral i) bArmyStats } 
  rank (URPosition { bArmyStat = bAS }) = fromIntegral . M.findIndex bAS $ bArmyStats

-- guard on promotion feasibility
guardRanking :: URPosition -> Ranking URPosition
guardRanking pos = Ranking size cmp unrank rank where
  URPosition { fixp = ep, wArmyStat = ArmyStat wtot wp wproms _, bArmyStat = ArmyStat btot bp bproms _ } = pos
  caps = 32 - wtot - btot
  np = 8 - ep
  wpx = np-wp-wproms  -- white pawns captured
  bpx = np-bp-bproms  -- black pawns captured
  maxuwp = bpx+caps-wproms  -- unopposed white pawns
  maxubp = wpx+caps-bproms  -- unopposed black pawns
  -- wp-maxuwp = wp-np+bp+bproms-caps+wproms
  minopp = ep + wp-maxuwp -- wp-maxuwp == bp-maxubp where maxubp = wpx + caps - bproms
  size = if min maxuwp maxubp >= 0 then 1 else 0
  cmp _ _ = EQ
  unrank _ = pos { minOpp = minopp }
  rank _ = 0

-- rank white army piece counts
wArmyRanking :: URPosition -> Ranking URPosition
wArmyRanking pos = Ranking size cmp unrank rank where
  URPosition { fixwr = fwr, fixp = ep, wArmyStat = wAS } = pos
  wArmies = armyStats fwr ep M.! wAS
  size = fromIntegral $ length wArmies
  cmp = compare `on` rank
  unrank i = pos { pieceCnts = cnts } where cnts = wArmies!!(fromIntegral i)
  rank (URPosition { pieceCnts = cnts }) = fromIntegral . fromJust . elemIndex (take 5 cnts) $ wArmies

-- rank black army piece counts
bArmyRanking :: URPosition -> Ranking URPosition
bArmyRanking pos = Ranking size cmp unrank rank where
  URPosition { fixbr = fbr, fixp = ep, bArmyStat = bAS , pieceCnts = wCnts } = pos
  bArmies = armyStats fbr ep M.! bAS
  size = fromIntegral $ length bArmies
  cmp = compare `on` rank
  unrank i = pos { pieceCnts = take 5 wCnts ++ cnts } where cnts = bArmies!!(fromIntegral i)
  rank (URPosition { pieceCnts = cnts }) = fromIntegral . fromJust . elemIndex (drop 5 cnts) $ bArmies

-- rank castling details
castleRanking :: URPosition -> Ranking URPosition
castleRanking pos = Ranking size cmp unrank rank where
  URPosition { whiteToMove = wtm, fixp = ep, fixwr = fwr, fixbr = fbr } = pos
  -- rookAssocs = filterMap cM [(0,'r'),(7,'r'),(56,'R'),(63,'R')]
  size = fromIntegral $ mwr * mbr
  mwr = multFR fwr 
  mbr = multFR fbr 
  multFR 0 = 1
  multFR 1 = 2
  multFR 2 = 1
  cmp = compare `on` rank
  unrank i' = pos { castleMap = cM } where
    i = fromIntegral i' :: Int
    wi = i `div` mbr
    bi = i `mod` mbr
    cM = (fCM fwr wi `shiftL` 2) .|. fCM fbr bi
    fCM 0 _ = 0
    fCM 1 i = 1+i
    fCM 2 _ = 3
  rank (URPosition { castleMap = cM }) = fromIntegral $ wi * mbr + bi where
     wi = if fwr == 1 then (cM `shiftR` 2) - 1 else 0
     bi = if fbr == 1 then (cM .&. 3) - 1 else 0

-- rank en-passant details
enPassantRanking :: URPosition -> Ranking URPosition
enPassantRanking pos = Ranking size cmp unrank rank where
  URPosition { whiteToMove = wtm, fixp = ep } = pos
  size = multEP ep
  multEP 0 = 1
  multEP 1 = 14
  cmp = compare `on` rank
  unrank i' = if ep==0 then pos else pos { wEP = wep, bEP = bep } where
    i = fromIntegral i'
    i7 = i `div` 2
    i2 = i `mod` 2
    row = if wtm then 24 else 32
    (wep, bep) = (row + i7 + i2, row + i7 + 1-i2)
  rank (URPosition { wEP = wep, bEP = bep }) = if ep==0 then 0 else fromIntegral $ i7 * 2 + i2 where
    wfile = wep `mod` 8
    bfile = bep `mod` 8
    i7 = (wfile + bfile) `div` 2
    i2 = wfile - i7

-- rank opposition of en-passant files
epOppRanking :: URPosition -> Ranking URPosition
epOppRanking pos@(URPosition { fixp = ep, wArmyStat = ArmyStat _ wp _ _, bArmyStat = ArmyStat _ bp _ _, minOpp = mo }) = Ranking size cmp unrank rank where
  opptions = if ep==0 then [(0,0)] else [(wopp,bopp) | wopp <- [max 0 (mo-wp)..1], bopp <- [max 0 (mo-bp)..1], wopp+bopp <= max 0 mo]
  size = fromIntegral $ length opptions
  cmp = compare `on` rank
  unrank i = pos { wEPOpp = wopp, bEPOpp = bopp } where (wopp,bopp) = opptions!!fromIntegral i
  rank (URPosition { wEPOpp = wopp, bEPOpp = bopp }) = fromIntegral . fromJust . elemIndex (wopp,bopp) $ opptions
 
-- rank amount of pawn free sandwich space
sandwichRanking :: URPosition -> Ranking URPosition
sandwichRanking pos@(URPosition { minOpp = mo }) = Ranking size cmp unrank rank where
  size = 1 + 4 * fromIntegral (max 0 mo)
  cmp = compare `on` rank
  unrank i = pos { oppSep = fromIntegral i }
  rank (URPosition { oppSep = s }) = fromIntegral s
 
-- opps p s n counts ways for p opposing pawns to sandwich s others in n files
opps :: Int -> Int -> Int -> Int
opps 0 0 _ = 1  -- done
opps 0 _ _ = 0  -- short of sandwiched space
opps _ _ 0 = 0  -- no space left for pawns
opps p s n = mopps p s (n-1) + sum [(5-i) * mopps (p-1) (s-i) (n-1) | i <- [0..min s 4]]

-- precomputed version
mopps :: Int -> Int -> Int -> Int
mopps p s n = mopps_!(p,s,n) where
mopps_ = array ((0,0,0),(8,32,8)) [((p,s,n), opps p s n) | p<-[0..8], s<-[0..32], n<-[0..8]] where

-- merge two file sorted opposing lists
merge :: [Opposing] -> [Opposing] -> [Opposing]
merge xs [] = xs
merge [] ys = ys
merge xs@(x@(xc,_):xs') ys@(y@(yc,_):ys') = if xc`mod`8 < yc`mod`8 then x:merge xs' ys else y:merge xs ys'

-- rank placement of opposing pawns
opposeRanking :: URPosition -> Ranking URPosition
opposeRanking pos = Ranking sz cmp unrnk rnk where
  URPosition { whiteToMove = wtm, fixp = ep, minOpp = mo, wEP = wep, bEP = bep, wEPOpp = wopp, bEPOpp = bopp, oppSep = s } = pos
  p = max 0 mo
  sz = if ep == 0 then fromIntegral (mopps p s 8) else size epR
  cmp = compare `on` rnk
  epR = sizeRanking $ epRCase wopp bopp where
    epRCase 0 0  = [([], epopp 0 0)]
    epRCase 0 1  = [([(bep,bep+8+ds*8)], epopp 1 ds) | ds<-[0,1..5-bep`div`8]]
    epRCase 1 0  = [([(wep-8-8*ds,wep)], epopp 1 ds) | ds<-[0,1..wep`div`8-2]]
    epRCase 1 1  = [(merge [(bep,bep+dr1*8)] [(wep-8*dr2,wep)], epopp 2 (dr1+dr2-2)) | dr1<-[1,2..6-bep`div`8], dr2<-[1,2..wep`div`8-1]] where
    epopp dp ds = if ds > s then 0 else fromIntegral $ mopps (p-dp) (s-ds) 6
  unrnk i = pos { oppList = (if ep == 0 then placeOpp [0..7] p s else placeEPOpp) i } where
    placeEPOpp i = merge epOpps (placeOpp fs p' s' i') where
      (epOpps, i') = unrank epR i
      fs = [0..7] \\ [wep `mod` 8, bep `mod` 8]
      p' = p - length epOpps
      s' = s - sum [(w-8 - b) `div` 8 | (b,w) <- epOpps]
    placeOpp []     0 _ 0 = []
    placeOpp (f:fs) p s i = if i < size0 then placeOpp fs p s i else setOpp (fileOpps f s) (i - size0) where
      size0 = fromIntegral $ mopps p s n
      n = length fs
      setOpp (opp@(b,w):opps) i = if i < size'
       then opp : placeOpp fs (p-1) s' i
       else setOpp opps (i - size') where
         size' = fromIntegral $ mopps (p-1) s' n 
         s' = s - ((w-8 - b) `div` 8)
  rnk (URPosition { oppList = oL }) = if ep == 0 then unplaceOpp [0..7] s oL else unplaceEPOpp where
    unplaceEPOpp = rank epR (epOpps, unplaceOpp fs s' opps) where
      (epOpps, opps) = partition (\(b,w) -> b == bep || w == wep) oL
      fs = [0..7] \\ [wep `mod` 8, bep `mod` 8]
      s' = s - sum [(w-8 - b) `div` 8 | (b,w) <- epOpps]
    unplaceOpp _      0 [] = 0
    unplaceOpp (f:fs) s os@(opp@(b,w):opps') = if b `mod` 8 /= f then unplaceOpp fs s os else sizeNo + rnkOpp (fileOpps f s) where
      sizeNo = fromIntegral $ mopps p s n
      p = length os
      n = length fs
      rnkOpp (fopp@(fb,fw):fopps) = if fopp == opp
       then unplaceOpp fs  s' opps'
       else size' + rnkOpp fopps where
         size' = fromIntegral $ mopps (p-1) s' n 
         s' = s - ((fw-8 - fb) `div` 8)
  fileOpps f s = [(b,w) | b <- [f+8,f+16..f+40], w <- [b+8,b+16..min (b+8+s*8) (f+48)]]
 
-- precompute first 65x29 falling powers into array for efficiency
fallingPower :: Int -> Int -> Integer
fallingPower n k = if n<0 then 0 else fp_!(n,k)
fp_ = array ((0,0),(64,28)) $ do
  n <- [0..64]
  ((n,0), 1) : [((n,k+1), fallingPower n k * fromIntegral (n-k)) | k<-[0..27]]
 
-- precompute first 63 factorials into array for efficiency
fac :: Int -> Integer
fac n = fac_!n

fac_ = listArray (0,64) (scanl (*) 1 [1..64])

-- rank placement of free pawns
pawnRanking :: URPosition -> Ranking URPosition
pawnRanking pos = Ranking size cmp unrnk rnk where
  URPosition { fixp = ep, minOpp = mo, wEP = wep, bEP = bep, wEPOpp = wepopp, bEPOpp = bepopp, oppSep = oS, oppList = oL, wArmyStat = ArmyStat _ wp _ _, bArmyStat = ArmyStat _ bp _ _ } = pos
  size   = fallingPower pspace (wp' + bp') `div` (fac wp' * fac bp')
  cmp = compare `on` rnk
  wpOpp = max 0 mo - wepopp
  bpOpp = max 0 mo - bepopp
  wp' = wp - wpOpp
  bp' = bp - bpOpp
  pspace = 48 - 4 * ep - wpOpp - bpOpp - oS
  pawns  = [(emptySymbol, pspace - wp' - bp'), ('P', wp'), ('p', bp')]
  multiR = multinomialRanking pawns
  unrnk i = pos { pawnString = unrank multiR i }
  rnk (URPosition { pawnString = pstr }) = rank multiR pstr

-- rank placement of free pieces
pieceRanking :: URPosition -> Ranking URPosition
pieceRanking pos = Ranking size cmp unrnk rnk where
  URPosition { fixp = ep, wArmyStat = ArmyStat wtot _ _ _, bArmyStat = ArmyStat btot _ _ _, pieceCnts = pcs } = pos
  size   = fallingPower pcspace (sum pcs) `div` product (map fac pcs)
  cmp = compare `on` rnk
  emptySpace = 64 - 2 * ep - wtot - btot
  pcspace = emptySpace + sum pcs
  pieces = (emptySymbol, emptySpace) : zip pieceSymbols pcs
  multiR = multinomialRanking pieces
  unrnk i = pos { pieceString = unrank multiR i } where
  rnk (URPosition { pieceString = pcString}) = rank multiR pcString

-- Chess Position Ranking
cpr :: Ranking URPosition
cpr = sideToMoveRanking `composeURI` (caseRanking `composeRI` wArmyStatRanking `composeURI` bArmyStatRanking `composeRI` guardRanking `composeRI` enPassantRanking `composeURI` epOppRanking `composeURI` sandwichRanking `composeRI` opposeRanking `composeURI` pawnRanking `composeURI` castleRanking `composeURI` wArmyRanking `composeURI` bArmyRanking `composeURI` pieceRanking) $ emptyURPosition
