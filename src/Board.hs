module Board where  -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO ANY CHANGES TO package.yaml, e.g.:
--       import Data.Chars
-- ab hier alles wurde von ChatGPT übernommen -- Außer die vorgegebene Codes
import Data.List()
import Data.Char (chr)
-- #############################################################################
-- ############# GIVEN IMPLEMENTATION                           ################
-- ############# Note: "deriving Show" may be deleted if needed ################
-- #############       Given data types may NOT be changed      ################
-- #############################################################################
data Player = Red | Blue deriving Show
data Cell =  Stack [Player] | Empty deriving Show
data Pos = Pos { col :: Char, row :: Int } deriving Show
data Dir = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest deriving Show
type Board = [[Cell]]

instance Eq Pos where
  (==) (Pos c1 r1) (Pos c2 r2) = (c1 == c2) && (r1 == r2)

instance Eq Player where  
  (==) Blue Blue = True
  (==) Red Red = True
  (==) _ _ = False

instance Eq Cell where
  (==) Empty Empty = True
  (==) (Stack xs) (Stack ys) = xs == ys
  (==) _ _ = False




-- Hilfsfunktion, um einen String bei jedem Vorkommen eines Charakters zu teilen
-- ChatGPT- Titel: PDF nicht lesbar. Hilfe? , Seite: 197
splitOn :: Char -> String -> [String]
splitOn _ "" = [""]
splitOn delimiter str =
    let (word, rest) = break (== delimiter) str
    in word : case rest of
        [] -> []
        x -> splitOn delimiter (tail x)

-- Überprüft, ob ein Feld gültig ist
-- ChatGPT- Titel: PDF nicht lesbar. Hilfe? , Seite: 197
isValidField :: String -> Bool
isValidField field = all (\c -> c == 'r' || c == 'b') field

-- Überprüft, ob eine Reihe gültig ist
-- ChatGPT- Titel: PDF nicht lesbar. Hilfe? , Seit 197:
isValidRow :: String -> Bool
isValidRow row = all isValidField (splitOn ',' row)

-- #############################################################################
-- ################# IMPLEMENT validateFEN :: String -> Bool ###################
-- ################## - 2 Functional Points                  ###################
-- ################## - 1 Coverage Point                     ###################
-- #############################################################################
-- ChatGPT- Titel: PDF nicht lesbar. Hilfe? , Seite:
validateFEN :: String -> Bool
validateFEN fen = 
  let
    rows = splitOn '/' fen
    isValidRow row = all (\c -> c `elem` ['r', 'b', ',']) row && length (splitOn ',' row) == 6
  in length rows == 6 && all isValidRow rows

-- #############################################################################
-- ####################### buildBoard :: String -> Board #######################
-- ####################### - 2 Functional Points         #######################
-- ####################### - 1 Coverage Point            #######################
-- #############################################################################
-- ChatGPT- Titel: Haskell Death Stacks v1 , Seite: 98

buildBoard :: String -> Board
buildBoard fen
  | validateFEN fen = map (map cellFromString . splitOn ',') $ splitOn '/' fen
  | otherwise       = error "Invalid FEN string"

cellFromString :: String -> Cell
cellFromString "" = Empty
cellFromString s  = Stack $ map charToPlayer s

charToPlayer :: Char -> Player
charToPlayer 'r' = Red
charToPlayer 'b' = Blue
charToPlayer _   = error "Invalid character"


-- #############################################################################
-- #################### path :: Pos -> Dir -> Int -> [Pos]  ####################
-- #################### - 4 Functional Points               ####################
-- #################### - 1 Coverage Point                  ####################
-- #############################################################################
-- ChatGPT- Titel: Haskell Code for Path and possibleMoves . Seite:30-44
path :: Pos -> Dir -> Int -> [Pos]
path startPos dir steps = pathHelper startPos dir steps []

pathHelper :: Pos -> Dir -> Int -> [Pos] -> [Pos]
pathHelper pos dir steps acc
  | steps == 0 = reverse (pos : acc)
  | pos == Pos 'f' 1 && isNorthEast dir && steps > 0 = pathHelper (Pos 'e' 2) NorthWest (steps - 1) (pos : acc)
  | pos == Pos 'a' 1 && isNorthWest dir && steps > 0 = pathHelper (Pos 'b' 2) NorthEast (steps - 1) (pos : acc)
  | pos == Pos 'a' 6 && isSouthWest dir && steps > 0 = pathHelper (Pos 'b' 5) SouthEast (steps - 1) (pos : acc)
  | pos == Pos 'f' 6 && isSouthEast dir && steps > 0 = pathHelper (Pos 'e' 5) SouthWest (steps - 1) (pos : acc)
  | isPositionValid nextPosition = pathHelper nextPosition (reflectDir pos dir) (steps - 1) (pos : acc)
  | otherwise = pathHelper pos (reverseDir dir) steps acc
  where nextPosition = nextPos dir pos

        isNorthEast :: Dir -> Bool
        isNorthEast NorthEast = True
        isNorthEast _ = False

        isNorthWest :: Dir -> Bool
        isNorthWest NorthWest = True
        isNorthWest _ = False

        isSouthWest :: Dir -> Bool
        isSouthWest SouthWest = True
        isSouthWest _ = False

        isSouthEast :: Dir -> Bool
        isSouthEast SouthEast = True
        isSouthEast _ = False

-- Rest of your code remains the same

-- Reflects the direction based on the third rule without using `==`
reflectDir :: Pos -> Dir -> Dir
reflectDir (Pos 'b' r) NorthWest
  | r >= 1 && r <= 4 = NorthEast
reflectDir (Pos 'b' r) SouthWest
  | r >= 3 && r <= 6 = SouthEast
reflectDir (Pos 'e' r) NorthEast
  | r >= 1 && r <= 4 = NorthWest
reflectDir (Pos 'e' r) SouthEast
  | r >= 3 && r <= 6 = SouthWest
reflectDir (Pos c 2) SouthEast
  | c >= 'b' && c <= 'd' = NorthEast
reflectDir (Pos c 2) SouthWest
  | c >= 'c' && c <= 'f' = NorthWest
reflectDir (Pos c 5) NorthEast
  | c >= 'a' && c <= 'd' = SouthEast
reflectDir (Pos c 5) NorthWest
  | c >= 'c' && c <= 'f' = SouthWest
reflectDir _ dir = dir  -- For all other positions, the direction remains the same

nextPos :: Dir -> Pos -> Pos
nextPos dir (Pos c r) = case dir of
    North     -> Pos c (r + 1)
    South     -> Pos c (r - 1)
    East      -> Pos (succ c) r
    West      -> Pos (pred c) r
    NorthEast -> Pos (succ c) (r + 1)
    SouthEast -> Pos (succ c) (r - 1)
    SouthWest -> Pos (pred c) (r - 1)
    NorthWest -> Pos (pred c) (r + 1)

isPositionValid :: Pos -> Bool
isPositionValid (Pos c r) = c >= 'a' && c <= 'f' && r >= 1 && r <= 6

reverseDir :: Dir -> Dir
reverseDir North = South
reverseDir South = North
reverseDir East = West
reverseDir West = East
reverseDir NorthEast = SouthWest
reverseDir SouthWest = NorthEast
reverseDir NorthWest = SouthEast
reverseDir SouthEast = NorthWest