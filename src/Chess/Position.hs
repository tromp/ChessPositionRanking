module Chess.Position (Coord, boardCoords, pawnCoords, CastleMap, filterMap, Square, emptySquare, Diagram, emptyDiagram, showDiagram, Position(..), emptyPosition, readFEN, writeFEN, printDiagram) where

import Data.Maybe
import Data.Char
import Data.List
import Data.List.Split
import Data.Array
import Data.Bits
 
-- ccordinate
-- integer in 0..63 corresponding to a8,b8,...,h8,a7,...,h7,...,a1,...,h1
type Coord = Int

-- common coordinate lists
boardCoords :: [Coord]
boardCoords = [0..63]

pawnCoords :: [Coord]
pawnCoords = [8..55]

-- filter list by bitmap, assuming 64 bit Ints
filterMap :: Int -> [a] -> [a]
filterMap 0 _ = []
filterMap i (x:xs) = let fm = filterMap (i `shiftR` 1) xs in if odd i then x:fm else fm

-- convert bitmap into list of set bit indices
bitmapCoords :: Int -> [Coord]
bitmapCoords bm = filterMap bm boardCoords

-- contents of a chess board square
type Square = Char

-- display a king queen rook bishop knight as character
emptySquare = '.'

-- a diagram is just an array of (64) squares
type Diagram = Array Coord Square

-- empty diagram
emptyDiagram :: Diagram
emptyDiagram = listArray (0, 63) (repeat emptySquare)

-- bitmap with bit 0 black queen(side), 1 black king, 2 white queen, 3 white king
type CastleMap = Int

-- position info corresponding to first 4 FEN fields
data Position = Position {
  diagram     :: Diagram,
  sideToMove  :: Char,
  castlings   :: CastleMap,
  enPassant   :: Coord
} deriving (Eq, Ord, Show)

-- empty position
emptyPosition :: Position
emptyPosition = Position {
  diagram     = listArray (0, 63) (repeat emptySquare),
  sideToMove  = 'w',
  castlings   = 0,
  enPassant   = 0
}

-- show a diagram as 8 lines of text
showDiagram :: Diagram -> String
showDiagram = concat . map ((++ ['\n']) . intersperse ' ') . chunksOf 8 . elems

-- run length encode pawns in FEN
compact :: String -> String
compact "" = ""
compact (c:s) = if c==emptySquare then show (1 + length empties) ++ compact rest else c: compact s where
  (empties, rest) = span (== emptySquare) s

-- convert a position to FEN
writeFEN :: Position -> String
writeFEN pos = board ++ " " ++ [stm] ++ " " ++ castles ++ " " ++ enpassants ++ " 0 1" where
  Position { diagram = d, sideToMove = stm, castlings = c, enPassant = ep } = pos
  board = intercalate "/" . map compact . chunksOf 8 . elems $ d
  castles = if c==0 then "-" else reverse $ filterMap c "qkQK"
  enpassants = if ep==0 then "-" else [fileChar, rowChar]
  fileChar = ['a'..'h'] !! (ep `mod` 8)
  rowChar =  intToDigit (8 - (ep `div` 8))

-- run length decode pawns in FEN
uncompact :: String -> String
uncompact "" = ""
uncompact (c:s) = uc ++ uncompact s where
  uc = if c=='/' then "" else if isDigit c then replicate (digitToInt c) emptySquare else [c]

-- read a FENboard sidetomove castlings enpassant
readFEN :: String -> Position
readFEN fen = case words fen of
  [board, [stm], castles, enpassant, _, _] -> let
     diag = listArray (0, 63) (uncompact board)
     cstl = if castles == "-" then 0 else sum . map ((1 `shiftL`) . fromJust . flip elemIndex "qkQK") $ castles
     ep = if enpassant=="-" then 0 else row * 8 + file where
       [fileChar, rowChar] = enpassant
       file = ord fileChar - ord 'a'
       row = 8 - digitToInt rowChar
    in Position { diagram = diag, sideToMove = stm, castlings = cstl, enPassant = ep }
  _ -> error $ "Not a FEN: " ++ fen

-- show position as string
-- instance Show Position where show = showDiagram . diagram
printDiagram :: Position -> IO ()
printDiagram = putStrLn . showDiagram . diagram
