module Main where

import System.Environment
import Chess.Position
import Data.Maybe
import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Array

-- the dark colored squares
darkSquares :: [Square]
darkSquares = filter isDark boardSquares where
  isDark c = let (y,x) = c `divMod` 8 in odd x /= odd y

-- spare room for promotions
spareProms :: Diagram -> (Int, String)
spareProms diag = let
  nkSymbols = "PQRBNpqrbn" -- non king squares
  zerocnts = zip nkSymbols (repeat 0)
  cnts = M.fromListWith (+) $ zerocnts ++ zip (elems diag) (repeat 1)
  nkcnts = map (cnts M.!) nkSymbols
  [wp,wq,wr,wb,wn,bp,bq,br,bb,bn] = nkcnts
  darkcnts = M.fromListWith (+) $ zerocnts ++ zip (map (diag!) darkSquares) (repeat 1)
  (pwb,pbb) = (bishopProms wb (darkcnts M.! 'B'), bishopProms bb (darkcnts M.! 'b')) where
    bishopProms tot dark = if dark > 0 && tot-dark > 0 then tot-2 else tot-1
  wproms = sum . map (max 0) $ [wq-1,wr-2,pwb,wn-2]
  bproms = sum . map (max 0) $ [bq-1,br-2,pbb,bn-2]
  tot = 2 + sum nkcnts -- add kings back
  wx = 15 - sum (take 5 nkcnts)
  bx = 15 - sum (drop 5 nkcnts)
  wpx = 8 - wp - wproms -- white pawns captured
  bpx = 8 - bp - bproms -- black pawns captured
  caps = 32 - tot
  maxuwp = bpx + caps - wproms
  maxubp = wpx + caps - bproms
  minopp = wp - maxuwp
  nOppFiles = length . filter (not . null) . mkOpps $ diag
  spare = if wpx >= 0 && bpx >= 0 then nOppFiles - minopp else -1
  stats = "\t wx " ++ show wx ++ " wp " ++ show wp ++ " wpr " ++ show wproms ++ " wpx " ++ show wpx ++ " maxuwp " ++ show maxuwp ++ " minopp " ++ show minopp ++
          "\t bx " ++ show bx ++ " bp " ++ show bp ++ " bpr " ++ show bproms ++ " bpx " ++ show bpx ++ " maxubp " ++ show maxubp ++ " minopp " ++ show minopp
 in (spare, stats)

-- list opponent pieces checking target coord
checks :: Diagram -> Square -> [(Symbol, (Int, Int))]
checks diag king = let
  (ky, kx) = king `divMod` 8
  onW = isUpper (diag!king)
  dP  = if onW then [(-1,-1),(1,-1)] else [(1,1),(-1,1)]
  dN  = [(2,1),(1,2),(-1,2),(-2,1),(-2,-1),(-1,-2),(1,-2),(2,-1)]
  dBQ = [(1,1),(-1,1),(-1,-1),(1,-1)]
  dRQ = [(1,0),(0,1),(-1,0),(0,-1)]
  toSquare (x,y) = y * 8 + x
  inRange x = x >= 0 && x < 8
  onBoard (x,y) = inRange x && inRange y
  ray (dx,dy) = map (\xy -> (diag!toSquare xy, xy)) . takeWhile onBoard . map (\i -> (kx+i*dx, ky+i*dy)) $ [1..] 
  neray = filter ((/= emptySymbol) . fst) . ray -- non empty ray
  toOpp = if onW then toLower else toUpper
 in [ x | (x@(sq,_):_) <- map   ray dP , sq == toOpp 'p'] ++
    [ x | (x@(sq,_):_) <- map   ray dN , sq == toOpp 'n'] ++
    [ x | (x@(sq,_):_) <- map neray dBQ, sq == toOpp 'b' || sq == toOpp 'q'] ++
    [ x | (x@(sq,_):_) <- map neray dRQ, sq == toOpp 'r' || sq == toOpp 'q']

-- research legality
research :: Position -> String
research (Position { diagram = diag, sideToMove = stm, castlings = c, enPassant = ep }) = let
  [wk, bk] = map (\sq -> fst . fromJust . find (\(i,x)-> x==sq) . assocs $ diag) "Kk"
  wtm = stm == 'w'
  (ktm,kntm) = if wtm then (wk,bk) else (bk,wk) -- to move or not to move
  [(yktm,  xktm), (ykntm, xkntm)] = map (`divMod` 8) [ktm, kntm]
  adjacentKings = abs (xktm - xkntm) <= 1 && abs (yktm - ykntm) <= 1
  (checksOnTM, checksOnNTM) = (checks diag ktm, checks diag kntm)
  (tmInCheck, ntmInCheck) = (not (null checksOnTM), not (null checksOnNTM))
  checkingpcs = sort . map (toLower . fst) $ checksOnTM
  [(x0,y0),(x1,y1)] = map (\(_,(x,y)) -> (x-xktm, y-yktm)) checksOnTM
  (innerProd, taxiCab) = (x0*x1 + y0*y1, sum (map abs [x0,y0,x1,y1]))
  (sharpAngle, rightAngle) = (innerProd > 0, innerProd == 0)
  minDist = M.fromList [("bb",99),("bn",7),("bp",99),("bq",5),("br",5),("nn",99),("np",99),("nq",6),("nr",6),("pp",99),("pq",5),("pr",5),("qq",99),("qr",5),("rr",99)]
  discoverable = taxiCab >= minDist M.! checkingpcs
  promRowCheckers = filter (\(_,(_,y)) -> y == if wtm then 7 else 0) checksOnTM 
  kDistToPromRow = if wtm then 7-yktm else yktm
  (slack, stats) = spareProms diag
  promRowLegal = case length promRowCheckers of
    0 ->  sharpAngle && discoverable 
    1 -> (rightAngle &&  taxiCab > 2  &&  kDistToPromRow <= 1 && slack > 0)
      || (sharpAngle && (discoverable ||  kDistToPromRow == 1))
    2 ->  sharpAngle && (discoverable || (kDistToPromRow == 2 && slack > 0))
  doubleCheck = if checkingpcs == "nn" then "Illegal Double Knight Check"
           else if promRowLegal then ("Discovered Double Check" ++ stats)
           else "Illegal Double Check"
 in if adjacentKings then "Illegal Adjacent Kings" else
    if tmInCheck && ntmInCheck then "Illegal Both Kings in Check" else
    if ntmInCheck then "Illegal Side not to move in Check" else
    if length checkingpcs > 2 then "Illegal Triple Check" else
    if slack < 0 then ("Illegal Bishops Too Monochromatic" ++ stats)  else
    if length checkingpcs == 2 then doubleCheck else
    (if length checkingpcs == 1 then "Single Check" else "No Checks") ++ stats

main = do
  let legality = map (\fen -> fen ++ "\t " ++ research (readFEN fen))
  getContents >>= mapM_ putStrLn . legality . filter (not . null) . lines
