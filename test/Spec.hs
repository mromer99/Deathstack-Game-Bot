-- #############################################################################
-- ####### YOUR UNIT TESTS                                           ###########
-- ####### Note: execute tests using "stack test deathstacks:units"  ###########
-- #############################################################################

-- ab hier alles wurde von ChatGPT übernommen -- Außer die vorgegebene Codes
import Test.Hspec
import Board (validateFEN, buildBoard, Cell(Empty, Stack), Player(Red, Blue), Pos(Pos), path, Dir(North), Dir(South), Dir(East), Dir(West), Dir(NorthEast), Dir(NorthWest), Dir(SouthEast), Dir(SouthWest))
import Control.Exception (evaluate)
import Deathstacks (playerWon, possibleMoves, Move(Move), target, start, isValidMove, steps, isPlayerTooTallStack, isTooTallStack, allBoardCells, listMoves)
import Data.List (nub)
import Board 
-- import sort (sort)
import Control.Exception (evaluate, ErrorCall)


boardContains :: Board -> Cell -> Bool
boardContains board cell = any (elem cell) board

showMoves :: [Move] -> [String]
showMoves = map show

isAllEven :: [Int] -> Bool
isAllEven lst = all even lst

main :: IO ()
main = hspec $ do

  describe "validateFEN" $ do

    -- Titel: Haskell Death Stacks v1 , Seite: 104-108    
    it "returns False for an empty string" $
      validateFEN "" `shouldBe` False

    it "returns False for a string with invalid row count" $
      validateFEN "r,b,r,b,r,b/" `shouldBe` False

    it "returns False for a string with invalid field count in a row" $
      validateFEN "r,b,r,b,r,b,r,b/,,,,,/,,,,,/,,,,,/,,,,,/,,,,," `shouldBe` False

    it "returns False for a string with invalid characters" $
      validateFEN "x,y,z,a,b,c/,,,,,/,,,,,/,,,,,/,,,,,/,,,,," `shouldBe` False

    it "returns False for correct structure but invalid contents" $
      validateFEN "rrb,bb,,/,,,,,/,,,,,/,,,,,/,,,,,/,,,,," `shouldBe` False

    it "returns True for a valid FEN string" $
      validateFEN "r,b,r,b,r,b/,,,,,/,,,,,/,,,,,/,,,,,/b,r,b,r,b,r" `shouldBe` True

    it "returns False for a string with too many fields in a row" $
      validateFEN "r,b,r,b,r,b,r/,,,,,/,,,,,/,,,,,/,,,,,/,,,,," `shouldBe` False

    it "returns False for a string with too many rows" $
      validateFEN "r,b,r,b,r,b/,,,,,/,,,,,/,,,,,/,,,,,/,,,,,/,,,,," `shouldBe` False

    it "returns False for a string with too few fields in a row" $
      validateFEN "r,b,r,b,r/,,,,,/,,,,,/,,,,,/,,,,,/,,,,," `shouldBe` False
  
    it "returns False for a string with invalid characters in fields" $
      validateFEN "r,x,r,b,r,b/,,,,,/,,,,,/,,,,,/,,,,,/,,,,," `shouldBe` False

    it "returns True for a valid FEN string representing current board situation" $
      validateFEN "r,b,,,b,r/,,,,,/b,,,,r,/,,,,,/r,,,b,,/,,,,," `shouldBe` True

    --- important tests for validateFEN -- those test was copied from validate runner GitLab
    -- Titel: Death Stacks -- Test cases , Seite: 4-5
    it "returns False for a string with not 6 rows" $
      validateFEN ",,,,,/,,,,," `shouldBe` False

    it "returns False for a string with not 6 columns" $
      validateFEN ",,,,,/,,,,,/,,,,,/,,,,,/,,,,/,,,,," `shouldBe` False

    it "returns True for a string with all empty fields" $
      validateFEN ",,,,,/,,,,,/,,,,,/,,,,,/,,,,,/,,,,," `shouldBe` True

    it "returns True for the starting board configuration" $
      validateFEN "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb" `shouldBe` True

    it "returns True for another valid position from the worksheet" $
      validateFEN "rr,,,,,rr/,,,,,/,bbr,rr,,rrb,/,,,,,/,,,,,/bb,bb,,,bbrrrb,bb" `shouldBe` True

    it "returns False for a board containing an invalid character" $
      validateFEN "rr,rr,rr,rr,rr,rw/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb" `shouldBe` False

    it "returns True for a complex field that is unknown to students but valid" $
      validateFEN ",,br,rr,r,r/b,,,bbbr,,r/,,,br,,/,b,,,,/,b,rrr,,rbb,b/,,,,b," `shouldBe` True

  describe "buildBoard" $ do
    -- Titel: Haskell Death Stacks v1 , Seite, 108 - 110
    it "creates a board with a mix of empty and occupied cells from a valid FEN string" $
      buildBoard "r,b,,,b,r/,,,,,/b,,,,r,/,,,,,/r,,,b,,/,,,,," `shouldBe` [
        [Stack [Red], Stack [Blue], Empty, Empty, Stack [Blue], Stack [Red]],
        replicate 6 Empty,
        [Stack [Blue], Empty, Empty, Empty, Stack [Red], Empty],
        replicate 6 Empty,
        [Stack [Red], Empty, Empty, Stack [Blue], Empty, Empty],
        replicate 6 Empty
      ]

    it "throws an error for an invalid FEN string" $
      evaluate (buildBoard "invalid FEN string") `shouldThrow` anyErrorCall

    it "correctly maps the FEN string positions to the board" $
      let board = buildBoard "r,,,,,/,,,,,/,,,,,/,,,,,/,,,,,/b,,,,,"
      in (board !! 0 !! 0 `shouldBe` Stack [Red]) >> (board !! 5 !! 0 `shouldBe` Stack [Blue])

    --- important tests for buildBoard -- those test was copied from validate runner GitLab
    --- Titel: Death Stacks -- Test cases , Seite: 5-6
    it "creates an empty board" $
      buildBoard ",,,,,/,,,,,/,,,,,/,,,,,/,,,,,/,,,,," `shouldBe` replicate 6 (replicate 6 Empty)

    it "creates the starting board" $
      buildBoard "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb" `shouldBe`
        [replicate 6 (Stack [Red, Red]), replicate 6 Empty, replicate 6 Empty, replicate 6 Empty, replicate 6 Empty, replicate 6 (Stack [Blue, Blue])]

    it "creates a valid board configuration 1" $
      buildBoard "rr,brr,,,,/,b,,r,rbbr,br/,,brr,,,brr/,,b,,,/,,,,,/,,,,bb,bb" `shouldBe`
        [[Stack [Red, Red], Stack [Blue, Red, Red], Empty, Empty, Empty, Empty], 
        [Empty, Stack [Blue], Empty, Stack [Red], Stack [Red, Blue, Blue, Red], Stack [Blue, Red]],
        [Empty, Empty, Stack [Blue, Red, Red], Empty, Empty, Stack [Blue, Red, Red]],
        [Empty, Empty, Stack [Blue], Empty, Empty, Empty],
        replicate 6 Empty,
        replicate 4 Empty ++ [Stack [Blue, Blue], Stack [Blue, Blue]]
        ] 

    it "creates a valid board configuration 2" $
      buildBoard ",r,,,brr,/,,br,,,/,,br,brrr,,/,,,br,br,/br,brb,,,,/,,,bb,b," `shouldBe`
        [[Empty, Stack [Red], Empty, Empty, Stack [Blue, Red, Red], Empty],
        [Empty, Empty, Stack [Blue, Red], Empty, Empty, Empty],
        [Empty, Empty, Stack [Blue, Red], Stack [Blue, Red, Red, Red], Empty, Empty],
        [Empty, Empty, Empty, Stack [Blue, Red], Stack [Blue, Red], Empty],
        [Stack [Blue, Red], Stack [Blue, Red, Blue], Empty, Empty, Empty, Empty],
        [Empty, Empty, Empty, Stack [Blue, Blue], Stack [Blue], Empty]] 

    it "checks if the board contains an empty cell" $
      boardContains (buildBoard "rr,brr,,,,/,b,,r,rbbr,br/,,brr,,,brr/,,b,,,/,,,,,/,,,,bb,bb") Empty `shouldBe` True

    it "checks if the board contains a red stack" $
      boardContains (buildBoard "rr,brr,,,,/,b,,r,rbbr,br/,,brr,,,brr/,,b,,,/,,,,,/,,,,bb,bb") (Stack [Red, Red]) `shouldBe` True

    it "checks if the board contains a blue stack" $
      boardContains (buildBoard "rr,brr,,,,/,b,,r,rbbr,br/,,brr,,,brr/,,b,,,/,,,,,/,,,,bb,bb") (Stack [Blue]) `shouldBe` True

    it "checks if the board contains a mixed stack" $
      boardContains (buildBoard "rr,brr,,,,/,b,,r,rbbr,br/,,brr,,,rbr/,,b,,,/,,,,,/,,,,bb,bb") (Stack [Blue, Red, Red]) `shouldBe` True

  describe "path" $ do
    -- Titel: Haskell Death Stacks v1 , Seite: 111 - 114
    it "creates a path moving north" $
      path (Pos 'c' 3) North 2 `shouldBe` [Pos 'c' 3, Pos 'c' 4, Pos 'c' 5]

    it "creates a path moving south" $
      path (Pos 'd' 4) South 11 `shouldBe` [Pos 'd' 4, Pos 'd' 3, Pos 'd' 2, Pos 'd' 1, Pos 'd' 2, Pos 'd' 3, Pos 'd' 4, Pos 'd' 5, Pos 'd' 6, Pos 'd' 5, Pos 'd' 4, Pos 'd' 3]
  
    it "creates a path moving east" $
      path (Pos 'b' 2) East 3 `shouldBe` [Pos 'b' 2, Pos 'c' 2, Pos 'd' 2, Pos 'e' 2]
    
    it "creates a path moving west" $
      path (Pos 'e' 5) West 5 `shouldNotBe` [Pos 'e' 5, Pos 'd' 5, Pos 'c' 5, Pos 'b' 5, Pos 'a' 5, Pos 'a' 5]

    it "creates a path moving west" $
      path (Pos 'e' 5) West 4 `shouldBe` [Pos 'e' 5, Pos 'd' 5, Pos 'c' 5, Pos 'b' 5, Pos 'a' 5]

    it "creates a path moving northeast" $
      path (Pos 'a' 1) NorthEast 2 `shouldBe` [Pos 'a' 1, Pos 'b' 2, Pos 'c' 3]

    it "creates a path with zero steps (stays at the starting position)" $
      path (Pos 'f' 6) South 0 `shouldBe` [Pos 'f' 6]

    it "creates a path with zero steps (stays at the starting position)" $
      path (Pos 'f' 6) South 0 `shouldBe` [Pos 'f' 6]

    it "creates a path to Southwest at e5" $
      path (Pos 'e' 5) SouthWest 4 `shouldBe` [Pos 'e' 5, Pos 'd' 4, Pos 'c' 3, Pos 'b' 2, Pos 'a' 1]

    it "creates a path that reflects off the board edges" $
      path (Pos 'f' 6) East 2 `shouldBe` [Pos 'f' 6, Pos 'e' 6, Pos 'd' 6]  

    it "doesn't creates a path that leaves the board" $
      path (Pos 'd' 5) NorthEast 3 `shouldNotBe` [Pos 'd' 5, Pos 'e' 6, Pos 'f' 7, Pos 'g' 8]

    it "doesn't create a path that leaves the board-f6" $
      path (Pos 'f' 6) East 1 `shouldNotBe` [Pos 'f' 6, Pos 'g' 6]  

    it "reflects off the top edge" $
      path (Pos 'c' 5) North 2 `shouldBe` [Pos 'c' 5, Pos 'c' 6, Pos 'c' 5]

    it "reflects off the bottom edge" $
      path (Pos 'd' 2) South 3 `shouldBe` [Pos 'd' 2, Pos 'd' 1, Pos 'd' 2, Pos 'd' 3]

    it "reflects off the right edge" $
      path (Pos 'e' 3) East 2 `shouldBe` [Pos 'e' 3, Pos 'f' 3, Pos 'e' 3]

    it "reflects off the left edge" $
      path (Pos 'b' 4) West 3 `shouldBe` [Pos 'b' 4, Pos 'a' 4, Pos 'b' 4, Pos 'c' 4]

    it "reflects diagonally off the northwest corner" $
      path (Pos 'b' 5) NorthWest 3 `shouldBe` [Pos 'b' 5, Pos 'a' 6, Pos 'b' 5, Pos 'c' 4]

    it "creates a path that reflects from Northeast to Northwest" $
      path (Pos 'e' 1) NorthEast 5 `shouldBe` [Pos 'e' 1, Pos 'f' 2, Pos 'e' 3, Pos 'd' 4, Pos 'c' 5, Pos 'b' 6]

    it "creates a path that reflects from northwest to southeast" $
      path (Pos 'b' 5) NorthWest 14 `shouldBe` [Pos 'b' 5, Pos 'a' 6, Pos 'b' 5, Pos 'c' 4, Pos 'd' 3, Pos 'e' 2, Pos 'f' 1, Pos 'e' 2, Pos 'd' 3, Pos 'c' 4, Pos 'b' 5, Pos 'a' 6, Pos 'b' 5, Pos 'c' 4, Pos 'd' 3]

    it "reflects from northwest to northeast" $
      path (Pos 'b' 4) NorthWest 2 `shouldBe` [Pos 'b' 4, Pos 'a' 5, Pos 'b' 6]

    it "reflects diagonally from northeast to Southeast" $
      path (Pos 'a' 4) NorthEast 5 `shouldBe` [Pos 'a' 4, Pos 'b' 5, Pos 'c' 6, Pos 'd' 5, Pos 'e' 4, Pos 'f' 3]

    it "reflects diagonally from southwest to southeast" $
      path (Pos 'b' 5) SouthWest 5 `shouldBe` [Pos 'b' 5, Pos 'a' 4, Pos 'b' 3, Pos 'c' 2, Pos 'd' 1, Pos 'e' 2]

    it "reflects diagonally from southeast to northeast" $ 
      path (Pos 'c' 3) SouthEast 8 `shouldBe` [Pos 'c' 3, Pos 'd' 2, Pos 'e' 1, Pos 'f' 2, Pos 'e' 3, Pos 'd' 4, Pos 'c' 5, Pos 'b' 6, Pos 'a' 5]

    it "reflects diagonally from southeast to northeast -- Large steps" $
      path (Pos 'b' 2) SouthEast 17 `shouldBe` [Pos 'b' 2, Pos 'c' 1, Pos 'd' 2, Pos 'e' 3, Pos 'f' 4, Pos 'e' 5, Pos 'd' 6, Pos 'c' 5, Pos 'b' 4, Pos 'a' 3, Pos 'b' 2, Pos 'c' 1, Pos 'd' 2, Pos 'e' 3, Pos 'f' 4, Pos 'e' 5, Pos 'd' 6, Pos 'c' 5] 

    it "reflects - c3 direction to NorthWest" $ 
      path (Pos 'c' 3) NorthWest 4 `shouldBe` [Pos 'c' 3, Pos 'b' 4, Pos 'a' 5, Pos 'b' 6, Pos 'c' 5]

        --- important tests for buildBoard -- those test was copied from validate runner GitLab
    -- Titel: Death Stacks -- Test cases , Seite: 8-9
    it "creates a simple path moving north" $
      path (Pos 'c' 2) North 1 `shouldBe` [Pos 'c' 2, Pos 'c' 3]

    it "verifies a simple north path contains the correct positions" $
      Pos 'c' 3 `shouldSatisfy` (`elem` path (Pos 'c' 2) North 1)

    it "creates a simple path moving northeast" $
      path (Pos 'a' 1) NorthEast 5 `shouldBe` [Pos 'a' 1, Pos 'b' 2, Pos 'c' 3, Pos 'd' 4, Pos 'e' 5, Pos 'f' 6]

    it "creates a simple path moving east" $
      path (Pos 'a' 6) East 2 `shouldBe` [Pos 'a' 6, Pos 'b' 6, Pos 'c' 6]

    it "creates a simple path moving southeast" $
      path (Pos 'b' 3) SouthEast 2 `shouldBe` [Pos 'b' 3, Pos 'c' 2, Pos 'd' 1]

    it "creates a simple path moving south" $
      path (Pos 'd' 6) South 4 `shouldBe` [Pos 'd' 6, Pos 'd' 5, Pos 'd' 4, Pos 'd' 3, Pos 'd' 2]

    it "creates a simple path moving southwest" $
      path (Pos 'e' 2) SouthWest 1 `shouldBe` [Pos 'e' 2, Pos 'd' 1]

    it "creates a simple path moving west" $
      path (Pos 'f' 1) West 3 `shouldBe` [Pos 'f' 1, Pos 'e' 1, Pos 'd' 1, Pos 'c' 1]

    it "creates a simple path moving northwest" $
      path (Pos 'b' 5) NorthWest 1 `shouldBe` [Pos 'b' 5, Pos 'a' 6]

    it "creates a mirrored path moving north" $
      path (Pos 'd' 5) North 2 `shouldBe` [Pos 'd' 5, Pos 'd' 6, Pos 'd' 5]

    it "creates a mirrored path moving south" $
      path (Pos 'd' 5) South 3 `shouldBe` [Pos 'd' 5, Pos 'd' 4, Pos 'd' 3, Pos 'd' 2]
      
    it "creates a mirrored path moving east with bounce" $
      path (Pos 'e' 5) East 8 `shouldBe` [Pos 'e' 5, Pos 'f' 5, Pos 'e' 5, Pos 'd' 5, Pos 'c' 5, Pos 'b' 5, Pos 'a' 5, Pos 'b' 5, Pos 'c' 5]

    it "creates a mirrored path moving northwest diagonally" $
      path (Pos 'c' 5) NorthWest 8 `shouldBe` [Pos 'c' 5, Pos 'b' 6, Pos 'a' 5, Pos 'b' 4, Pos 'c' 3, Pos 'd' 2, Pos 'e' 1, Pos 'f' 2, Pos 'e' 3]

    it "creates a mirrored path moving northeast diagonally" $
      path (Pos 'a' 2) NorthEast 6 `shouldBe` [Pos 'a' 2, Pos 'b' 3, Pos 'c' 4, Pos 'd' 5, Pos 'e' 6, Pos 'f' 5, Pos 'e' 4]

    it "creates a mirrored path in the corner moving northeast" $
      path (Pos 'f' 6) NorthEast 2 `shouldBe` [Pos 'f' 6, Pos 'e' 5, Pos 'd' 4]

  
    --- Tests to cover Coverage--
  -- Titel: Death Stacks -- Test cases , Seite: 9-13
  describe "Coverage" $ do 
    
    it "shows Red player correctly" $
      show Red `shouldBe` "Red"

    it "shows Blue player correctly" $
      show Blue `shouldBe` "Blue"

    it "shows an Empty cell correctly" $
      show Empty `shouldBe` "Empty"

    it "shows a Cell with a Red stack correctly" $
      show (Stack [Red]) `shouldBe` "Stack [Red]"

    it "shows a Cell with a mixed stack correctly" $
      show (Stack [Red, Blue]) `shouldBe` "Stack [Red,Blue]"

    it "shows a position correctly" $
      show (Pos 'c' 3) `shouldBe` "Pos {col = 'c', row = 3}"  

    it "correctly accesses the column field of a position" $
      col (Pos 'c' 3) `shouldBe` 'c'

    it "correctly accesses the row field of a position" $
      row (Pos 'c' 3) `shouldBe` 3

    it "correctly compares two Red players for equality" $
      (Red == Red) `shouldBe` True

    it "correctly compares two Blue players for equality" $
      (Blue == Blue) `shouldBe` True

    it "correctly compares a Red and a Blue player for inequality" $
      (Red == Blue) `shouldBe` False

    it "correctly compares a Blue and a Red player for inequality" $
      (Blue == Red) `shouldBe` False

    it "explicitly uses (==) to compare two Red players for equality" $
      Red `shouldBe` Red

    it "explicitly uses (==) to compare two Blue players for equality" $
      Blue `shouldBe` Blue

    it "explicitly uses (/=) to compare a Red and a Blue player for inequality" $
      Red `shouldNotBe` Blue

    it "explicitly uses (/=) to compare a Blue and a Red player for inequality" $
      Blue `shouldNotBe` Red

    it "correctly compares two Empty cells for equality" $
      (Empty == Empty) `shouldBe` True

    it "correctly compares two identical Stack cells for equality" $
      (Stack [Red, Blue] == Stack [Red, Blue]) `shouldBe` True

    it "correctly compares two different Stack cells for inequality" $
      (Stack [Red] == Stack [Blue]) `shouldBe` False

    it "correctly compares a Stack and an Empty cell for inequality" $
      (Stack [Red, Blue] == Empty) `shouldBe` False

    it "correctly compares an Empty cell and a Stack for inequality" $
      (Empty == Stack [Red, Blue]) `shouldBe` False

    it "correctly compares two Empty cells for equality using shouldBe" $
      Empty `shouldBe` Empty

    it "correctly compares two identical Stack cells for equality using shouldBe" $
      Stack [Red, Blue] `shouldBe` Stack [Red, Blue]

    it "correctly compares two different Stack cells for inequality using shouldNotBe" $
      Stack [Red] `shouldNotBe` Stack [Blue]

    it "correctly compares a Stack and an Empty cell for inequality using shouldNotBe" $
      Stack [Red, Blue] `shouldNotBe` Empty

    it "correctly compares an Empty cell and a Stack for inequality using shouldNotBe" $
      Empty `shouldNotBe` Stack [Red, Blue]

    it "returns True for a field with only 'r' and 'b'" $
      isValidField "rrbbbrb" `shouldBe` True

    it "returns False for a field with characters other than 'r' and 'b'" $
      isValidField "rbcx" `shouldBe` False

    it "returns True for a valid row with only 'r' and 'b'" $
      isValidRow "rr,bb,rb,br,b,r" `shouldBe` True

    it "throws an error for an invalid character" $ do
      evaluate (charToPlayer 'x') `shouldThrow` anyErrorCall

    -- Titel: Death Stacks -- Test cases , Seite: 19:20
    it "correctly executes pathHelper for specific condition at Pos 'f' 1 with NorthEast direction" $ do
      let startPosition = Pos 'f' 6
      let direction = SouthEast 
      let initialSteps = 2 -- 
      let expectedResult = [Pos 'f' 6, Pos 'e' 5, Pos 'd' 4] 
      pathHelper startPosition direction initialSteps [] `shouldBe` expectedResult

    it "correctly executes pathHelper for specific condition at Pos 'f' 1 with NorthEast direction" $ do
      let startPosition = Pos 'a' 1
      let direction = NorthWest 
      let initialSteps = 2 -- 
      let expectedResult = [Pos 'a' 1, Pos 'b' 2, Pos 'c' 3] 
      pathHelper startPosition direction initialSteps [] `shouldBe` expectedResult

    it "correctly executes pathHelper for specific condition at Pos 'f' 1 with NorthEast direction" $ do
      let startPosition = Pos 'a' 6
      let direction = SouthWest 
      let initialSteps = 2 -- 
      let expectedResult = [Pos 'a' 6, Pos 'b' 5, Pos 'c' 4] 
      pathHelper startPosition direction initialSteps [] `shouldBe` expectedResult

    it "executes pathHelper with NorthWest direction from Pos 'f' 1 in NorthEast direction" $ do
      let startPosition = Pos 'f' 1
      let direction = NorthEast 
      let initialSteps = 2 
      let expectedResult = [Pos 'f' 1, Pos 'e' 2, Pos 'd' 3] 
      pathHelper startPosition direction initialSteps [] `shouldBe` expectedResult
-------------------------------------------

----------------------------------------------------------------------------------------------------------------------------
--- ######################################################################################################################################################################################################
---------------- Tests for Deathstacks starting here ------------------------------------------------------------------------------------------

  describe "playerWon" $ do
    -- Titel: Haskell Death Stacks v1 , Seite: 117
    it "returns Nothing when the game is still open" $
      playerWon [[Empty, Stack [Red], Empty], [Empty, Stack [Blue], Empty], [Empty, Stack [Red], Empty]] `shouldBe` Nothing

    it "returns Just Red when Red controls all stacks" $
      playerWon (replicate 3 (replicate 3 (Stack [Red]))) `shouldBe` Just Red

    it "returns Just Blue when Blue controls all stacks" $
      playerWon (replicate 3 (replicate 3 (Stack [Blue]))) `shouldBe` Just Blue

    it "returns Nothing when stacks are controlled by both players" $
      playerWon [[Stack [Red], Stack [Blue], Stack [Red]], [Stack [Blue], Stack [Red], Stack [Blue]], [Stack [Red], Stack [Blue], Stack [Red]]] `shouldBe` Nothing



  describe "possibleMoves" $ do
    -- Titel: Haskell Death Stacks v1 , Seite: 120 - 121
    it "returns an empty list for an empty stack" $
      possibleMoves (Pos 'c' 3) Empty `shouldBe` []

    it "returns moves in all directions for a stack with one piece" $
      possibleMoves (Pos 'c' 3) (Stack [Red]) `shouldMatchList` 
        [Move (Pos 'c' 3) (Pos 'c' 4) 1, Move (Pos 'c' 3) (Pos 'c' 2) 1, Move (Pos 'c' 3) (Pos 'd' 3) 1, Move (Pos 'c' 3) (Pos 'b' 3) 1, 
        Move (Pos 'c' 3) (Pos 'd' 4) 1, Move (Pos 'c' 3) (Pos 'd' 2) 1, Move (Pos 'c' 3) (Pos 'b' 4) 1, Move (Pos 'c' 3) (Pos 'b' 2) 1]
    
    it "returns moves for multiple steps in one direction for a larger stack" $
      possibleMoves (Pos 'b' 2) (Stack [Red, Blue]) `shouldContain` [Move (Pos 'b' 2) (Pos 'b' 4) 2]

    it "returns a move with 9 steps" $
      possibleMoves (Pos 'a' 1) (Stack [Red, Red, Red, Red, Red, Red, Red, Red, Red, Red, Red]) `shouldContain` [Move (Pos 'a' 1) (Pos 'b' 1)  11]

    -- infinite loop möglich
    it "does return moves that end up in the same position" $
      possibleMoves (Pos 'a' 1) (Stack [Red]) `shouldNotContain` [Move (Pos 'a' 1) (Pos 'a' 1) 1] -- infinite loop

    it "does not return moves that end up in the same position" $
      possibleMoves (Pos 'c' 3) (Stack [Red, Blue, Blue]) `shouldNotContain` [Move (Pos 'c' 3) (Pos 'c' 3) 3]


    it "only returns moves that result in a change of state" $
      let moves = possibleMoves (Pos 'c' 3) (Stack [Red, Red, Blue])
      in all (\move -> target move /= start move) moves `shouldBe` True

    it "does not return duplicate moves" $
      let moves = possibleMoves (Pos 'd' 4) (Stack [Blue, Blue, Red])
      in length moves == length (nub moves) `shouldBe` True
    

    it "returns correct moves for possibleMoves (Pos 'e' 1) (Stack [Red, Red, Red, Red])" $ do
      let moves = possibleMoves (Pos 'e' 1) (Stack [Red, Red, Red, Red])
      let expectedMoves = [
            "e1-1-e2", "e1-1-f2", "e1-1-f1", "e1-1-d2", "e1-1-d1",
            "e1-2-e3", "e1-2-c3", "e1-2-c1", "e1-3-e4", 
            "e1-3-d4", "e1-3-d1", "e1-3-b4", "e1-3-b1", 
            "e1-4-e5", "e1-4-c5", "e1-4-c1", "e1-4-a5", "e1-4-a1"]
      map show moves `shouldMatchList` expectedMoves
    
    it "returns correct moves for possibleMoves (Pos 'c' 3) (Stack [Red, Red, Red, Red])" $ do
      let moves = possibleMoves (Pos 'c' 3) (Stack [Red, Red, Red, Red])
      let expectedMoves = [
            "c3-1-b2", "c3-1-b3", "c3-1-b4", "c3-1-c2", "c3-1-c4", "c3-1-d2", "c3-1-d3", "c3-1-d4",
            "c3-2-a1", "c3-2-a3", "c3-2-a5", "c3-2-c1", "c3-2-c5", "c3-2-e1", "c3-2-e3", "c3-2-e5",
            "c3-3-b2", "c3-3-b3", "c3-3-b6", "c3-3-c2", "c3-3-c6", "c3-3-f2", "c3-3-f3", "c3-3-f6",
            "c3-4-e5", "c3-4-e3", "c3-4-c5"]
      map show moves `shouldMatchList` expectedMoves

    it "returns correct moves for possibleMoves (Pos 'c' 3) (Stack [Red, Red, Blue, Red, Red])" $ do
      let moves = possibleMoves (Pos 'c' 3) (Stack [Red, Red, Blue, Red, Red])
      let expectedMoves = [
            "c3-1-b2", "c3-1-b3", "c3-1-b4", "c3-1-c2", "c3-1-c4", "c3-1-d2", "c3-1-d3", "c3-1-d4",
            "c3-2-a1", "c3-2-a3", "c3-2-a5", "c3-2-c1", "c3-2-c5", "c3-2-e1", "c3-2-e3", "c3-2-e5",
            "c3-3-b2", "c3-3-b3", "c3-3-b6", "c3-3-c2", "c3-3-c6", "c3-3-f2", "c3-3-f3", "c3-3-f6",
            "c3-4-e5", "c3-4-e3", "c3-4-c5",
            "c3-5-c4", "c3-5-d3", "c3-5-d4"]
      map show moves `shouldMatchList` expectedMoves  

    it "returns correct moves for possibleMoves (Pos 'c' 6) (Stack [Red, Red, Blue, Blue, Blue])" $ do
      let moves = possibleMoves (Pos 'c' 6) (Stack [Red, Red, Blue, Blue, Blue])
      let expectedMoves = [
            "c6-1-b5", "c6-1-b6", "c6-1-c5", "c6-1-d6", "c6-1-d5",
            "c6-2-a4", "c6-2-a6", "c6-2-c4", "c6-2-e4", "c6-2-e6",
            "c6-3-b3", "c6-3-b6", "c6-3-c3", "c6-3-f3", "c6-3-f6",
            "c6-4-e2", "c6-4-e6", "c6-4-c2",
            "c6-5-c1", "c6-5-d1", "c6-5-d6"]
      map show moves `shouldMatchList` expectedMoves

    it "returns correct moves for possibleMoves (Pos 'b' 5) (Stack [Blue, Red, Blue, Blue, Blue, Blue, Red, Red, Blue, Red])" $ do
      let moves = possibleMoves (Pos 'b' 5) (Stack [Blue, Red, Blue, Blue, Blue, Blue, Red, Red, Blue, Red])
      let expectedMoves = [
            "b5-1-b6", "b5-1-c6", "b5-1-c5", "b5-1-c4", "b5-1-b4", "b5-1-a4", "b5-1-a5", "b5-1-a6",
            "b5-2-d5", "b5-2-d3", "b5-2-b3",
            "b5-3-b2", "b5-3-b4", "b5-3-c2", "b5-3-c5", "b5-3-c4", "b5-3-e5", "b5-3-e2", "b5-3-e4",
            "b5-4-b3", "b5-4-f5", "b5-4-f3", "b5-4-f1", "b5-4-b1", "b5-4-d5", "b5-4-d3", "b5-4-d1", 
            "b5-5-b2", "b5-5-e5", "b5-5-e2", "b5-6-b1",
            "b5-6-d5", "b5-6-d3", "b5-6-d1", "b5-6-b3", "b5-6-f5", "b5-6-f3", "b5-6-f1",
            "b5-7-b2", "b5-7-b4", "b5-7-c5", "b5-7-c4", "b5-7-c2", "b5-7-e5", "b5-7-e4","b5-7-e2",
            "b5-8-b3", "b5-8-d5", "b5-8-d3", "b5-9-b4", "b5-9-a4", "b5-9-a5", "b5-9-a6", "b5-9-b6", "b5-9-c6", "b5-9-c5", "b5-9-c4"]
      map show moves `shouldMatchList` expectedMoves

    it "returns moves in all directions for a stack in the middle of the board" $
      let moves = possibleMoves (Pos 'd' 4) (Stack [Blue])
      in length moves `shouldBe` 8  

    it "returns moves that reflect off the board edges" $
      let moves = possibleMoves (Pos 'f' 3) (Stack [Blue, Red])
      in any (\move -> target move == Pos 'e' 3) moves `shouldBe` True

  -- Titel: Haskell Death Stacks v1 , Seite: 132
    it "returns moves with reflection for a stack at the edge of the board" $
      let moves = possibleMoves (Pos 'f' 3) (Stack [Red, Blue])
      in any (\m -> target m == Pos 'e' 3) moves `shouldBe` True

    it "returns moves for a larger stack in one direction" $
      let moves = possibleMoves (Pos 'b' 2) (Stack [Red, Blue, Blue])
      in any (\m -> target m == Pos 'b' 5) moves `shouldBe` True

  describe "isValidMove" $ do
    it "returns False for a move that is not possible" $
      isValidMove (replicate 6 (replicate 6 Empty)) (Move (Pos 'c' 3) (Pos 'c' 4) 1) `shouldBe` False

    it "returns False for a move that is not possible" $
      isValidMove (replicate 6 (replicate 6 Empty)) (Move (Pos 'c' 3) (Pos 'c' 4) 3) `shouldBe` False

    it "returns False for a move that is not possible" $
      isValidMove (replicate 6 (replicate 6 Empty)) (Move (Pos 'c' 3) (Pos 'c' 4) 4) `shouldBe` False

    it "returns False for a move that is not possible" $
      isValidMove (replicate 6 (replicate 6 Empty)) (Move (Pos 'c' 3) (Pos 'c' 4) 5) `shouldBe` False

    it "returns True for a simple Move" $
      let board = buildBoard "r,b,r,b,r,b/,,,,,/,,,,,/,,,,,/,,,,,/b,r,b,r,b,r"
      in isValidMove board (Move (Pos 'e' 1) (Pos 'f' 1) 1) `shouldBe` True

    --- seleber geschrieben
    it "returns True for a simple Move - 1" $
      let board = buildBoard ",,rr,,,/,,,,,/,,,,,/,,,,,brr/,bb,,brr,,/rrr,,,,,"
      in isValidMove board (Move (Pos 'f' 3) (Pos 'd' 3) 2) `shouldBe` True
    
    it "returns True for a simple Move - 2" $
      let board = buildBoard "rr,rr,rr,rr,rr,/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bbb,b"
      in isValidMove board (Move (Pos 'e' 1) (Pos 'd' 4) 3) `shouldBe` True

    it "returns True for a simple Move - 3" $
      let board = buildBoard "rr,rr,rr,rr,rr,/,,,,,/,,,,,/,,,,,/,,,,,/bbbbb,bb,bb,bb,bbb,b"
      in isValidMove board (Move (Pos 'a' 1) (Pos 'f' 1) 5)

    it "returns True for a Move based on Too-Tall-Rule" $
      let board = buildBoard ",,rr,,,/,rbbbbr,,,,/,,,,,/,,,,,brrr/,bb,,brr,,/rrrrrrrrr,,,,,"
      in isValidMove board (Move (Pos 'b' 5) (Pos 'd' 5) 6) `shouldBe` True
  
    it "returns True for a Move based on Too-Tall-Rule - 1" $
      let board = buildBoard ",,rr,,,/,rbbbbr,,,,/,,,,,/,,,,,brrr/,bb,,brr,,/rrrrrrrrr,,,,,"
      in isValidMove board (Move (Pos 'a' 1) (Pos 'c' 1) 8) `shouldBe` True
    --- bis hier----

    it "returns False for a move that is not stacksize > 4 and there is another stack > stackSize4 " $
      let board = buildBoard "rr,rr,r,,rrr,/,,,,,/,,,,,/,,,,,/,,,,,/bbbbb,,,b,bbb,b"
      in isValidMove board (Move (Pos 'e' 1) (Pos 'd' 4) 3) `shouldBe` False

    it "returns False for a Move based on Too-Tall-Rule " $
      let board = buildBoard ",,rr,,,/,rbbbbr,,,,/,,,,,/,,,,,brrr/,bb,,brr,,/brrrrrrrr,,,,,"
      in isValidMove board (Move (Pos 'a' 1) (Pos 'c' 1) 2) `shouldBe` False


  describe "listMoves" $ do
     
      --- Tests to cover Coverage--

    it "returns an empty list when no moves are possible for the given player" $ do
      let board = replicate 6 (replicate 6 Empty) -- All cells are empty
      let player = Red
      listMoves board player `shouldBe` []

  describe "Coverage" $ do
    -- Titel: Death Stacks -- Test cases , Seite: 14-29
    it "correctly accesses the steps field of a Move" $
      steps (Move (Pos 'a' 1) (Pos 'b' 2) 3) `shouldBe` 3

    it "correctly shows a Move instance" $ do
      let move = Move (Pos 'a' 1) (Pos 'b' 2) 3
      let expectedString = "a1-3-b2"
      show move `shouldBe` expectedString

    it "matches the string representation of a Move instance" $ do
      let move = show (Move (Pos 'a' 1) (Pos 'b' 2) 3)
      case move of
        "a1-3-b2" -> True `shouldBe` True
        _         -> error "Show instance for Move did not match expected format"    

    it "correctly shows a single Move instance" $ do
      let singleMove = Move (Pos 'a' 1) (Pos 'b' 2) 3
      show singleMove `shouldBe` "a1-3-b2"

    it "correctly shows a list of Move instances" $ do
      let movesList = [Move (Pos 'a' 1) (Pos 'b' 2) 3, Move (Pos 'c' 4) (Pos 'd' 5) 6]
      show movesList `shouldBe` "[a1-3-b2,c4-6-d5]"

    it "confirms that two identical Move instances are equal" $ do
      let move1 = Move (Pos 'a' 1) (Pos 'b' 2) 3
      let move2 = Move (Pos 'a' 1) (Pos 'b' 2) 3
      move1 `shouldBe` move2

    it "confirms that two different Move instances are not equal" $ do
      let move1 = Move (Pos 'a' 1) (Pos 'b' 2) 3
      let move2 = Move (Pos 'c' 4) (Pos 'd' 5) 6
      move1 `shouldNotBe` move2

    it "confirms inequality for Moves with different start positions" $ do
      let move1 = Move (Pos 'a' 1) (Pos 'b' 2) 3
      let move2 = Move (Pos 'c' 1) (Pos 'b' 2) 3
      move1 `shouldNotBe` move2

    it "confirms inequality for Moves with different target positions" $ do
      let move1 = Move (Pos 'a' 1) (Pos 'b' 2) 3
      let move2 = Move (Pos 'a' 1) (Pos 'd' 5) 3
      move1 `shouldNotBe` move2

    it "confirms inequality for Moves with different steps" $ do
      let move1 = Move (Pos 'a' 1) (Pos 'b' 2) 3
      let move2 = Move (Pos 'a' 1) (Pos 'b' 2) 4
      move1 `shouldNotBe` move2

    it "returns True for a too tall stack of the same player" $ do
      let cell = Stack [Red, Red, Red, Red, Red]
      isPlayerTooTallStack cell Red `shouldBe` True

    it "returns False for a too tall stack of a different player" $ do
      let cell = Stack [Blue, Blue, Blue, Blue, Blue]
      isPlayerTooTallStack cell Red `shouldBe` False

    it "returns False for a short stack of the same player" $ do
      let cell = Stack [Red, Red, Red]
      isPlayerTooTallStack cell Red `shouldBe` False

    it "returns True for a mixed too tall stack where the top player matches" $ do
      let cell = Stack [Red, Blue, Red, Blue, Red]
      isPlayerTooTallStack cell Red `shouldBe` True

    it "returns False for an empty stack" $ do
      let cell = Stack []
      isPlayerTooTallStack cell Red `shouldBe` False

    it "returns True for a stack with exactly 5 players where the top player matches" $ do
      let cell = Stack [Red, Red, Red, Red, Red]
      isPlayerTooTallStack cell Red `shouldBe` True

    it "returns True for a mixed stack where the top player matches and stack is too tall" $ do
      let cell = Stack [Red, Blue, Red, Blue, Red, Blue]
      isPlayerTooTallStack cell Red `shouldBe` True

    it "returns False for a mixed stack where the top player does not match" $ do
      let cell = Stack [Blue, Red, Red, Red, Red, Red]
      isPlayerTooTallStack cell Red `shouldBe` False

    it "returns False for a stack with less than 5 players" $ do
      let cell = Stack [Red, Red, Red, Red]
      isPlayerTooTallStack cell Red `shouldBe` False

    it "returns True for a stack taller than four" $ do
      let cell = Stack [Red, Red, Red, Red, Red] -- A stack with more than four players
      isTooTallStack cell `shouldBe` True

    it "returns False for a stack with exactly four players" $ do
      let cell = Stack [Red, Red, Red, Red] -- A stack with exactly four players
      isTooTallStack cell `shouldBe` False

    it "returns False for a stack with fewer than four players" $ do
      let cell = Stack [Red, Red, Red] -- A stack with fewer than four players
      isTooTallStack cell `shouldBe` False

    it "returns False for an empty stack" $ do
      let cell = Stack [] -- An empty stack
      isTooTallStack cell `shouldBe` False

    it "returns False for a non-stack cell (Empty)" $ do
      let cell = Empty
      isTooTallStack cell `shouldBe` False

    it "correctly enumerates all board cells with their positions" $ do
      let board = [[Empty, Stack [Red]], [Stack [Blue, Blue], Empty]] -- A simple 2x2 board
      let expectedCells = [(Pos 'a' 1, Empty), (Pos 'b' 1, Stack [Red]), (Pos 'a' 2, Stack [Blue, Blue]), (Pos 'b' 2, Empty)]
      allBoardCells board `shouldBe` expectedCells
