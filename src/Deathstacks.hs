module Deathstacks where  -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
--       import Data.Char
---- ab hier alles wurde von ChatGPT übernommen -- Außer die vorgegebene Codes
import Board
import Data.Maybe (catMaybes, mapMaybe, fromJust) -- ich muss überprüfen ob das erlaubt ist
import Data.Char (chr, ord)
import Data.List
import Data.List (nubBy)
import Debug.Trace
import Data.List(foldl')
-- #############################################################################
-- ########################### GIVEN IMPLEMENTATION ############################
-- #############################################################################

data Move = Move {start :: Pos, target :: Pos, steps :: Int}

instance Show Move where
  show (Move (Pos startC startR) (Pos tarC tarR) tr) = [startC] ++ show startR ++ "-" ++ show tr ++ "-" ++ [tarC] ++ show tarR

instance Eq Move where
  (==) (Move (Pos sc1 sr1) (Pos tc1 tr1) r1) (Move (Pos sc2 sr2) (Pos tc2 tr2) r2) =
      sc1 == sc2 && sr1 == sr2 && tc1 == tc2 && tr1 == tr2 && r1 == r2 


-- #############################################################################
-- #################### playerWon :: Board -> Maybe Player #####################
-- #################### - 4 Functional Points              #####################
-- #################### - 1 Coverage Point                 #####################
-- #############################################################################

--- Titel: PDF nicht lesbar. Hilfe? , Seite:210
--playerWon :: Board -> Maybe Player
--playerWon board
  --  | allSameOwner Red board = Just Red
  --  | allSameOwner Blue board = Just Blue
  --  | otherwise = Nothing
--  where
--    allSameOwner player board = all (all (stackOwnedBy player)) board
--    stackOwnedBy player (Stack (x:_)) = x == player
--    stackOwnedBy _ _ = False


playerWon :: Board -> Maybe Player
playerWon board
    | allSameOwner Red board = Just Red
    | allSameOwner Blue board = Just Blue
    | otherwise = Nothing
  where
    allSameOwner player board = all (stackOwnedBy player) board
    stackOwnedBy :: Player -> [Cell] -> Bool
    stackOwnedBy player = all (\cell -> case cell of
                                        Stack (x:_) -> x == player
                                        Empty -> True 
                                        _ -> False)


-- #############################################################################
-- ################### possibleMoves :: Pos -> Cell -> [Move] ##################
-- ################### - 4 Functional Points                  ##################
-- ################### - 1 Coverage Point                     ##################
-- #############################################################################
-- Titel: Haskell Code for Path and possibleMoves, Seite: 31 - 43
possibleMoves :: Pos -> Cell -> [Move]
possibleMoves pos (Stack players) =
  nub $ concatMap (\dir -> movesInDirection pos dir (length players)) allDirections
  where
    allDirections = [North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest]

    movesInDirection :: Pos -> Dir -> Int -> [Move]
    movesInDirection startPos dir stackHeight =
        concat [if null positions || startPos == endPos || not (all isOnBoard positions) 
                then [] 
                else [Move startPos endPos steps]
                | steps <- [1..stackHeight], 
                  let positions = path startPos dir steps,
                  let endPos = last positions]
possibleMoves _ Empty = []

-- Helper function to check if a position is within the bounds of the board
isOnBoard :: Pos -> Bool
isOnBoard (Pos c r) = c >= 'a' && c <= 'f' && r >= 1 && r <= 6


-- #############################################################################
-- ################### isValidMove :: Board -> Move -> Bool ####################
-- ################### - 5 Functional Points                ####################
-- ################### - 1 Coverage Point                   ####################
-- #############################################################################
-- Titel: Haskell Code for Path and possibleMoves, Seite: 54
isValidMove :: Board -> Move -> Bool
isValidMove board move@(Move startPos targetPos steps) =
  case getCell board startPos of
    Stack players@(player : _) ->
      let stackHeight = length players
          playerHasTooTallStack = any (\(_, cell) -> isPlayerTooTallStack cell player) (allBoardCells board)
          moveIsFromTooTallStack = stackHeight > 4 && steps >= stackHeight - 4 && steps <= stackHeight
          moveIsFromShorterStack = not playerHasTooTallStack && steps <= stackHeight
      in moveIsFromTooTallStack || moveIsFromShorterStack
    _ -> False

-- Check if the cell is a too tall stack of the given player
isPlayerTooTallStack :: Cell -> Player -> Bool
isPlayerTooTallStack (Stack (p:ps)) player = p == player && length (p:ps) > 4
isPlayerTooTallStack _ _ = False


-- Check if the cell is a stack taller than four
isTooTallStack :: Cell -> Bool
isTooTallStack (Stack s) = length s > 4
isTooTallStack _ = False


allBoardCells :: Board -> [(Pos, Cell)]
allBoardCells board = [ (Pos c r, cell) | (r, row) <- zip [1..] board, (c, cell) <- zip ['a'..'f'] row ]

getCell :: Board -> Pos -> Cell
getCell board (Pos c r) = board !! (6 - r) !! (fromEnum c - fromEnum 'a')





-- ############# IMPLEMENT listMoves :: Board -> Player -> [Move] ##############
-- ############# - 2 Functional Points                            ##############
-- ############# - 1 Coverage Point                               ##############
-- #############################################################################
-- Titel: Haskell Code for Path and possibleMoves, Seite: 55
listMoves :: Board -> Player -> [Move]
listMoves board player = concatMap generateMovesForCell allBoardPositions
  where
    allBoardPositions = [Pos col row | col <- ['a'..'f'], row <- [1..6]]
    generateMovesForCell pos = case getCell board pos of
      Stack (topPlayer:_) | topPlayer == player ->
        filter (isValidMove board) (possibleMoves pos (getCell board pos))
      _ -> []

