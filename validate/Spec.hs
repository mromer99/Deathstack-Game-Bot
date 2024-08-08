
import Test.Hspec

import Board
    ( buildBoard,
      path,
      validateFEN,
      Board,
      Cell(Empty,Stack),
      Player(Red, Blue),
      Pos(Pos), Dir (North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest))
import Deathstacks ( playerWon, isValidMove, listMoves, Move(Move), possibleMoves )

main :: IO ()
main = hspec $ do
    testValidateFEN
    testBuildBoard
    testPath
    testPlayerWon
    testPossibleMoves
    testIsValidMove
    testListMoves

-- Helpers
boardContains :: Board -> Cell -> Bool
boardContains board cell = let flattened = foldl (++) [] board
    in foldl (\a b ->  a || (b == cell)) False flattened

sampleBoard :: Board
sampleBoard = [[Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]]]

testValidateFEN :: Spec
testValidateFEN = describe "IF Grade validateFEN 2FP" $ do
        it "empty string is not valid - TEST: validateFEN \"\"" $ do
            validateFEN "" `shouldBe` (False :: Bool)
        it "fen has not 6 rows - TEST: validateFEN \",,,,,/,,,,,\"" $ do
            validateFEN ",,,,,/,,,,," `shouldBe` (False :: Bool)
        it "fen has not 6 columns - TEST: validateFEN \",,,,,/,,,,,/,,,,,/,,,,,/,,,,/,,,,,\"" $ do
            validateFEN ",,,,,/,,,,,/,,,,,/,,,,,/,,,,/,,,,," `shouldBe` (False :: Bool)
        it "empty field is valid - TEST: validateFEN \",,,,,/,,,,,/,,,,,/,,,,,/,,,,,/,,,,,\"" $ do
            validateFEN ",,,,,/,,,,,/,,,,,/,,,,,/,,,,,/,,,,," `shouldBe` (True :: Bool)
        it "starting board is valid - TEST: validateFEN \"rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb\"" $ do
            validateFEN "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb" `shouldBe` (True :: Bool)
        it "another valid position from worksheet - TEST: validateFEN \"rr,,,,,rr/,,,,,/,bbr,rr,,rrb,/,,,,,/,,,,,/bb,bb,,,bbrrrb,bb\"" $ do
            validateFEN "rr,,,,,rr/,,,,,/,bbr,rr,,rrb,/,,,,,/,,,,,/bb,bb,,,bbrrrb,bb" `shouldBe` (True :: Bool)
        it "board contains invalid character -  TEST: validateFEN \"rr,rr,rr,rr,rr,rw/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb\"" $ do
            validateFEN "rr,rr,rr,rr,rr,rw/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb" `shouldBe` (False :: Bool)
        it "test field that is unknown to students - TEST: validateFEN \",,br,rr,r,r/b,,,bbbr,,r/,,,br,,/,b,,,,/,b,rrr,,rbb,b/,,,,b,\"" $ do
            validateFEN ",,br,rr,r,r/b,,,bbbr,,r/,,,br,,/,b,,,,/,b,rrr,,rbb,b/,,,,b," `shouldBe` (True :: Bool)

testBuildBoard :: Spec
testBuildBoard = describe "IF Grade buildBoard 2FP" $ do
        it "build empty board - TEST: buildBoard \",,,,,/,,,,,/,,,,,/,,,,,/,,,,,/,,,,,\"" $ do
            (buildBoard ",,,,,/,,,,,/,,,,,/,,,,,/,,,,,/,,,,,") `shouldBe` ([[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty]] :: Board)
        it "build starting board - TEST: buildBoard \"rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb\"" $ do
            (buildBoard "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb") `shouldBe` ([[Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]]] :: Board)
        it "buildBoard valid board 1 - TEST: buildBoard \"rr,brr,,,,/,b,,r,rbbr,br/,,brr,,,brr/,,b,,,/,,,,,/,,,,bb,bb\"" $ do
            (buildBoard "rr,brr,,,,/,b,,r,rbbr,br/,,brr,,,brr/,,b,,,/,,,,,/,,,,bb,bb") `shouldBe` ([[Stack [Red,Red],Stack [Blue,Red,Red],Empty,Empty,Empty,Empty], [Empty,Stack [Blue],Empty,Stack [Red],Stack [Red,Blue,Blue,Red],Stack [Blue,Red]], [Empty,Empty,Stack [Blue,Red,Red],Empty,Empty,Stack [Blue,Red,Red]], [Empty,Empty,Stack [Blue],Empty,Empty,Empty], [Empty,Empty,Empty,Empty,Empty,Empty], [Empty,Empty,Empty,Empty,Stack [Blue,Blue],Stack [Blue,Blue]]] :: Board)
        it "buildBoard valid board 2 - TEST: buildBoard \",r,,,brr,/,,br,,,/,,br,brrr,,/,,,br,br,/br,brb,,,,/,,,bb,b,\"" $ do
            (buildBoard ",r,,,brr,/,,br,,,/,,br,brrr,,/,,,br,br,/br,brb,,,,/,,,bb,b,") `shouldBe` ([[Empty,Stack [Red],Empty,Empty,Stack [Blue,Red,Red],Empty], [Empty,Empty,Stack [Blue,Red],Empty,Empty,Empty], [Empty,Empty,Stack [Blue,Red],Stack [Blue,Red,Red,Red],Empty,Empty], [Empty,Empty,Empty,Stack [Blue,Red],Stack [Blue,Red],Empty], [Stack [Blue,Red],Stack [Blue,Red,Blue],Empty,Empty,Empty,Empty], [Empty,Empty,Empty,Stack [Blue,Blue],Stack [Blue],Empty]] :: Board)
        it "buildBoard contains empty - TEST: boardContains (buildBoard \"rr,brr,,,,/,b,,r,rbbr,br/,,brr,,,brr/,,b,,,/,,,,,/,,,,bb,bb\") Empty" $ do
            (boardContains (buildBoard "rr,brr,,,,/,b,,r,rbbr,br/,,brr,,,brr/,,b,,,/,,,,,/,,,,bb,bb") Empty) `shouldBe` (True :: Bool)
        it "buildBoard contains red stack - TEST: boardContains (buildBoard \"rr,brr,,,,/,b,,r,rbbr,br/,,brr,,,brr/,,b,,,/,,,,,/,,,,bb,bb\") (Stack [Red,Red])" $ do
            (boardContains (buildBoard "rr,brr,,,,/,b,,r,rbbr,br/,,brr,,,brr/,,b,,,/,,,,,/,,,,bb,bb") (Stack [Red,Red])) `shouldBe` (True :: Bool)
        it "buildBoard contains blue stack - TEST: boardContains (buildBoard \"rr,brr,,,,/,b,,r,rbbr,br/,,brr,,,brr/,,b,,,/,,,,,/,,,,bb,bb\") (Stack [Blue])" $ do
            (boardContains (buildBoard "rr,brr,,,,/,b,,r,rbbr,br/,,brr,,,brr/,,b,,,/,,,,,/,,,,bb,bb") (Stack [Blue])) `shouldBe` (True :: Bool)
        it "buildBoard contains mixed stack - TEST: boardContains (buildBoard \"rr,brr,,,,/,b,,r,rbbr,br/,,brr,,,rbr/,,b,,,/,,,,,/,,,,bb,bb\") (Stack [Blue,Red,Red])" $ do
            (boardContains (buildBoard "rr,brr,,,,/,b,,r,rbbr,br/,,brr,,,rbr/,,b,,,/,,,,,/,,,,bb,bb") (Stack [Blue,Red,Red])) `shouldBe` (True :: Bool)

testPath :: Spec
testPath = describe "IF Grade path 3FP" $ do
        -- simple directions, no mirroring
        it "simple north -  TEST: path (Pos 'c' 2) North 1" $ do
            path (Pos 'c' 2) North 1 `shouldBe` ([(Pos 'c' 2), (Pos 'c' 3)] :: [Pos])
        it "simple north contains -  TEST: path (Pos 'c' 2) North 1" $ do
            path (Pos 'c' 2) North 1 `shouldContain` ([(Pos 'c' 3)] :: [Pos])
        it "simple northeast - TEST: path (Pos 'a' 1) NorthEast 5" $ do
            path (Pos 'a' 1) NorthEast 5 `shouldMatchList` ([(Pos 'a' 1), (Pos 'b' 2), (Pos 'c' 3), (Pos 'd' 4), (Pos 'e' 5), (Pos 'f' 6)] :: [Pos])
        it "simple east - TEST: path (Pos 'a' 6) East 2" $ do
            path (Pos 'a' 6) East 2 `shouldMatchList` ([(Pos 'a' 6), (Pos 'b' 6), (Pos 'c' 6)] :: [Pos])
        it "simple southeast - TEST: path (Pos 'b' 3) SouthEast 2" $ do
            path (Pos 'b' 3) SouthEast 2 `shouldContain` ([(Pos 'b' 3)] :: [Pos])
        it "simple south - TEST: path (Pos 'd' 6) South 4" $ do
            path (Pos 'd' 6) South 4 `shouldBe` ([(Pos 'd' 6), (Pos 'd' 5), (Pos 'd' 4), (Pos 'd' 3), (Pos 'd' 2)] :: [Pos])
        it "simple southwest - TEST: path (Pos 'e' 2) SouthWest 1" $ do
            path (Pos 'e' 2) SouthWest 1 `shouldMatchList` ([(Pos 'e' 2), (Pos 'd' 1)] :: [Pos])
        it "simple west - TEST: path (Pos 'f' 1) West 3" $ do
            path (Pos 'f' 1) West 3 `shouldContain` ([(Pos 'c' 1)] :: [Pos])    
        it "simple northwest - TEST: path (Pos 'b' 5) NorthWest 1" $ do
            path (Pos 'b' 5) NorthWest 1 `shouldBe` ([(Pos 'b' 5), (Pos 'a' 6)] :: [Pos])
        -- mirroring tests
        it "mirror north - TEST: path (Pos 'd' 5) North 2" $ do
            path (Pos 'd' 5) North 2 `shouldBe` ([(Pos 'd' 5),(Pos 'd' 6), (Pos 'd' 5)] :: [Pos])
        it "mirror south - TEST: path (Pos 'd' 5) South 3" $ do
            path (Pos 'd' 5) South 3 `shouldMatchList` ([(Pos 'd' 5),(Pos 'd' 4), (Pos 'd' 3), (Pos 'd' 2)] :: [Pos])
        it "mirror east west bounce - TEST: path (Pos 'e' 5) East 8:" $ do
            path (Pos 'e' 5) East 8 `shouldBe` ([(Pos 'e' 5),(Pos 'f' 5),(Pos 'e' 5),(Pos 'd' 5),(Pos 'c' 5),(Pos 'b' 5),(Pos 'a' 5),(Pos 'b' 5),(Pos 'c' 5)] :: [Pos])
        it "mirror diagonal 1 - TEST: path (Pos 'c' 5) NorthWest 8" $ do
            path (Pos 'c' 5) NorthWest 8 `shouldMatchList` ([(Pos 'c' 5),(Pos 'b' 6),(Pos 'a' 5),(Pos 'b' 4),(Pos 'c' 3),(Pos 'd' 2),(Pos 'e' 1),(Pos 'f' 2),(Pos 'e' 3)] :: [Pos])
        it "mirror diagonal 2 - TEST: path (Pos 'a' 2) NorthEast 6" $ do
            path (Pos 'a' 2) NorthEast 6 `shouldMatchList` ([(Pos 'a' 2),(Pos 'b' 3),(Pos 'c' 4),(Pos 'd' 5),(Pos 'e' 6),(Pos 'f' 5),(Pos 'e' 4)] :: [Pos])
        -- Corner mirror
        it "corner mirror - TEST: path (Pos 'f' 6) NorthEast 2" $ do
            path (Pos 'f' 6) NorthEast 2 `shouldBe` ([(Pos 'f' 6),(Pos 'e' 5),(Pos 'd' 4)] :: [Pos])


testPlayerWon :: Spec
testPlayerWon = describe "IF Grade playerWon 3FP" $ do
        it "start board not finished" $ do
            playerWon sampleBoard `shouldBe` (Nothing :: Maybe Player)
        it "board not finished 1" $ do
            let board = [[Empty,Stack [Red],Empty,Empty,Stack [Blue,Red,Red],Empty], [Empty,Empty,Stack [Blue,Red],Empty,Empty,Empty], [Empty,Empty,Stack [Blue,Red],Stack [Blue,Red,Red,Red],Empty,Empty], [Empty,Empty,Empty,Stack [Blue,Red],Stack [Blue,Red],Empty], [Stack [Blue,Red],Stack [Blue,Red,Blue],Empty,Empty,Empty,Empty], [Empty,Empty,Empty,Stack [Blue,Blue],Stack [Blue],Empty]] in
                playerWon board `shouldBe` (Nothing :: Maybe Player)
        it "board not finished 2" $ do
            let board = [[Stack [Blue,Red],Stack [Blue,Red],Empty,Empty,Stack [Blue,Red,Red],Empty], [Empty,Empty,Stack [Blue,Red,Blue],Empty,Stack [Blue,Red,Red],Empty], [Empty,Empty,Stack [Red,Red],Empty,Stack [Blue,Red,Blue,Blue,Blue],Empty], [Empty,Empty,Empty,Empty,Empty,Empty], [Stack [Red],Stack [Blue,Red,Blue],Empty,Empty,Empty,Empty], [Empty,Empty,Empty,Empty,Empty,Empty]] in
                playerWon board `shouldBe` (Nothing :: Maybe Player)
        it "Blue Won board 1" $ do
            let board = [[Stack [Blue,Red],Stack [Blue,Red],Empty,Empty,Stack [Blue,Red,Red],Empty], [Empty,Empty,Stack [Blue,Red,Blue],Empty,Stack [Blue,Red,Red],Empty], [Empty,Empty,Stack [Blue,Red,Red,Red],Empty,Stack [Blue],Empty], [Empty,Empty,Empty,Empty,Empty,Empty], [Empty,Stack [Blue,Red,Blue,Red,Blue],Empty,Empty,Empty,Empty], [Empty,Stack [Blue],Empty,Empty,Empty,Empty]] in
                playerWon board `shouldBe` (Just Blue :: Maybe Player)
        it "Blue Won board 2" $ do
            let board = [[Empty,Empty,Stack [Blue,Red,Red],Empty,Empty,Empty], [Empty,Empty,Stack [Blue,Red,Blue,Red],Stack [Blue,Red,Blue],Empty,Empty], [Stack [Blue,Red,Red,Red],Empty,Empty,Empty,Empty,Empty], [Empty,Stack [Blue],Empty,Empty,Stack [Blue,Red,Blue,Red,Red,Red],Empty], [Empty,Stack [Blue],Empty,Empty,Empty,Empty], [Stack [Blue],Empty,Empty,Stack [Blue],Empty,Empty]] in
                playerWon board `shouldBe` (Just Blue :: Maybe Player)
        it "Red Won board 1" $ do
            let board = [[Empty,Empty,Empty,Empty,Empty,Empty], [Empty,Empty,Stack [Red,Blue,Blue,Red],Empty,Empty,Empty], [Stack [Red,Red],Stack [Red,Blue],Stack [Red,Blue],Stack [Red,Blue,Red,Red],Empty,Empty], [Empty,Stack [Red,Blue,Blue],Empty,Empty,Empty,Empty], [Empty,Empty,Stack [Red,Blue,Blue,Blue],Stack [Red,Blue,Blue],Empty,Empty], [Empty,Empty,Empty,Empty,Empty,Empty]] in
                playerWon board `shouldBe` (Just Red :: Maybe Player)
        it "Red Won board 2" $ do
            let board = [[Empty,Stack [Red,Red,Red],Stack [Red,Blue,Blue],Empty,Empty,Empty], [Stack [Red,Red,Blue,Blue],Empty,Empty,Empty,Empty,Empty], [Stack [Red],Empty,Empty,Empty,Empty,Empty], [Empty,Empty,Empty,Empty,Stack [Red,Blue,Blue,Blue],Stack [Red,Blue,Blue]], [Empty,Empty,Empty,Stack [Red,Red],Empty,Empty], [Empty,Empty,Empty,Empty,Stack [Red,Blue,Blue,Blue],Empty]] in
                playerWon board `shouldBe` (Just Red :: Maybe Player)

testPossibleMoves :: Spec
testPossibleMoves = describe "IF Grade possibleMoves 6FP" $ do
        it "single test" $ do
            possibleMoves (Pos 'b' 1) (Stack [Red]) `shouldMatchList` ([Move (Pos 'b' 1) (Pos 'b' 2) 1,Move (Pos 'b' 1) (Pos 'c' 2) 1,Move (Pos 'b' 1) (Pos 'c' 1) 1,Move (Pos 'b' 1) (Pos 'a' 2) 1,Move (Pos 'b' 1) (Pos 'a' 1) 1] :: [Move])
        it "single red stack - all moves middle of field" $ do
            possibleMoves (Pos 'c' 3) (Stack [Red]) `shouldMatchList` ([Move (Pos 'c' 3) (Pos 'c' 4) 1,Move (Pos 'c' 3) (Pos 'd' 4) 1,Move (Pos 'c' 3) (Pos 'd' 3) 1,Move (Pos 'c' 3) (Pos 'd' 2) 1,Move (Pos 'c' 3) (Pos 'c' 2) 1,Move (Pos 'c' 3) (Pos 'b' 2) 1,Move (Pos 'c' 3) (Pos 'b' 3) 1,Move (Pos 'c' 3) (Pos 'b' 4) 1] :: [Move])
        it "single red stack - move north" $ do
            possibleMoves (Pos 'c' 3) (Stack [Red]) `shouldContain` ([Move (Pos 'c' 3) (Pos 'c' 4) 1] :: [Move])
        it "single red stack - move southwest" $ do
            possibleMoves (Pos 'd' 5) (Stack [Red]) `shouldContain` ([Move (Pos 'd' 5) (Pos 'c' 4) 1] :: [Move])
        it "single red check for completeness and duplicates" $ do
                possibleMoves (Pos 'b' 1) (Stack [Red]) `shouldMatchList` ([Move (Pos 'b' 1) (Pos 'b' 2) 1,Move (Pos 'b' 1) (Pos 'c' 2) 1,Move (Pos 'b' 1) (Pos 'c' 1) 1,Move (Pos 'b' 1) (Pos 'a' 2) 1,Move (Pos 'b' 1) (Pos 'a' 1) 1] :: [Move])
                length (possibleMoves (Pos 'b' 1) (Stack [Red])) `shouldSatisfy` (<= 5)
        it "multi red bounce south east" $ do
            possibleMoves (Pos 'e' 6) (Stack [Red, Red, Red]) `shouldContain` ([Move (Pos 'e' 6) (Pos 'd' 3) 3] :: [Move])
        it "multi red bounce east" $ do
            possibleMoves (Pos 'e' 6) (Stack [Red, Red, Red]) `shouldContain` ([Move (Pos 'e' 6) (Pos 'd' 6) 3] :: [Move])
        it "check tall stack red" $ do
            possibleMoves (Pos 'c' 3) (Stack [Red, Blue, Red, Blue, Red]) `shouldMatchList` ([Move (Pos 'c' 3) (Pos 'c' 4) 5,Move (Pos 'c' 3) (Pos 'c' 5) 4,Move (Pos 'c' 3) (Pos 'c' 6) 3,Move (Pos 'c' 3) (Pos 'c' 5) 2,Move (Pos 'c' 3) (Pos 'c' 4) 1,Move (Pos 'c' 3) (Pos 'd' 4) 5,Move (Pos 'c' 3) (Pos 'e' 5) 4,Move (Pos 'c' 3) (Pos 'f' 6) 3,Move (Pos 'c' 3) (Pos 'e' 5) 2,Move (Pos 'c' 3) (Pos 'd' 4) 1,Move (Pos 'c' 3) (Pos 'd' 3) 5,Move (Pos 'c' 3) (Pos 'e' 3) 4,Move (Pos 'c' 3) (Pos 'f' 3) 3,Move (Pos 'c' 3) (Pos 'e' 3) 2,Move (Pos 'c' 3) (Pos 'd' 3) 1,Move (Pos 'c' 3) (Pos 'f' 2) 3,Move (Pos 'c' 3) (Pos 'e' 1) 2,Move (Pos 'c' 3) (Pos 'd' 2) 1,Move (Pos 'c' 3) (Pos 'c' 2) 3,Move (Pos 'c' 3) (Pos 'c' 1) 2,Move (Pos 'c' 3) (Pos 'c' 2) 1,Move (Pos 'c' 3) (Pos 'b' 2) 3,Move (Pos 'c' 3) (Pos 'a' 1) 2,Move (Pos 'c' 3) (Pos 'b' 2) 1,Move (Pos 'c' 3) (Pos 'b' 3) 3,Move (Pos 'c' 3) (Pos 'a' 3) 2,Move (Pos 'c' 3) (Pos 'b' 3) 1,Move (Pos 'c' 3) (Pos 'b' 6) 3,Move (Pos 'c' 3) (Pos 'a' 5) 2,Move (Pos 'c' 3) (Pos 'b' 4) 1] :: [Move])
        it "check move red has to change board" $ do
            possibleMoves (Pos 'c' 3) (Stack [Red, Red, Red, Red, Red]) `shouldNotContain` ([Move (Pos 'c' 3) (Pos 'c' 3) 4] :: [Move])
        it "check blue stack mirror at corner" $ do
            possibleMoves (Pos 'b' 5) (Stack [Blue, Red, Red, Blue]) `shouldContain` ([Move (Pos 'b' 5) (Pos 'c' 4) 3] :: [Move])
        it "check blue stack in corner" $ do
            possibleMoves (Pos 'f' 1) (Stack [Blue, Blue, Blue, Blue]) `shouldMatchList` ([Move (Pos 'f' 1) (Pos 'f' 5) 4,Move (Pos 'f' 1) (Pos 'f' 4) 3,Move (Pos 'f' 1) (Pos 'f' 3) 2,Move (Pos 'f' 1) (Pos 'f' 2) 1,Move (Pos 'f' 1) (Pos 'b' 5) 4,Move (Pos 'f' 1) (Pos 'c' 4) 3,Move (Pos 'f' 1) (Pos 'd' 3) 2,Move (Pos 'f' 1) (Pos 'e' 2) 1,Move (Pos 'f' 1) (Pos 'b' 1) 4,Move (Pos 'f' 1) (Pos 'c' 1) 3,Move (Pos 'f' 1) (Pos 'd' 1) 2,Move (Pos 'f' 1) (Pos 'e' 1) 1] :: [Move])
        it "check blue stack north west" $ do
            possibleMoves (Pos 'e' 4) (Stack [Blue, Blue]) `shouldContain` ([Move (Pos 'e' 4) (Pos 'c' 6) 2] :: [Move])
        it "check blue stack north east with mirror" $ do
            possibleMoves (Pos 'e' 4) (Stack [Blue, Blue]) `shouldContain` ([Move (Pos 'e' 4) (Pos 'e' 6) 2] :: [Move])
        it "check wrong mirroring 1" $ do
            possibleMoves (Pos 'b' 6) (Stack [Blue, Red, Red, Blue]) `shouldNotContain` ([Move (Pos 'b' 6) (Pos 'b' 5) 2] :: [Move])
        it "check wrong mirroring 1" $ do
            possibleMoves (Pos 'c' 1) (Stack [Blue, Red, Blue]) `shouldNotContain` ([Move (Pos 'c' 1) (Pos 'c' 2) 2] :: [Move])
        it "check huge stack completenes and soundndess" $
            let expected = [Move (Pos 'c' 3) (Pos 'c' 2) 7,Move (Pos 'c' 3) (Pos 'c' 4) 5,Move (Pos 'c' 3) (Pos 'c' 5) 4,Move (Pos 'c' 3) (Pos 'c' 6) 3,Move (Pos 'c' 3) (Pos 'b' 2) 7,Move (Pos 'c' 3) (Pos 'd' 4) 5,Move (Pos 'c' 3) (Pos 'e' 5) 4,Move (Pos 'c' 3) (Pos 'f' 6) 3,Move (Pos 'c' 3) (Pos 'b' 3) 7,Move (Pos 'c' 3) (Pos 'd' 3) 5,Move (Pos 'c' 3) (Pos 'e' 3) 4,Move (Pos 'c' 3) (Pos 'f' 3) 3,Move (Pos 'c' 3) (Pos 'b' 6) 7,Move (Pos 'c' 3) (Pos 'c' 5) 6,Move (Pos 'c' 3) (Pos 'f' 2) 3,Move (Pos 'c' 3) (Pos 'c' 6) 7,Move (Pos 'c' 3) (Pos 'c' 2) 3,Move (Pos 'c' 3) (Pos 'f' 6) 7,Move (Pos 'c' 3) (Pos 'e' 5) 6,Move (Pos 'c' 3) (Pos 'b' 2) 3,Move (Pos 'c' 3) (Pos 'f' 3) 7,Move (Pos 'c' 3) (Pos 'e' 3) 6,Move (Pos 'c' 3) (Pos 'b' 3) 3,Move (Pos 'c' 3) (Pos 'f' 2) 7,Move (Pos 'c' 3) (Pos 'b' 6) 3]
                found = possibleMoves (Pos 'c' 3) (Stack [Blue, Blue, Red, Blue, Red, Blue, Blue])
                in do
                    and (map (`elem` found) expected) `shouldBe` (True :: Bool)
                    length found `shouldSatisfy` (>= 25)

testIsValidMove :: Spec
testIsValidMove = describe "IF Grade isValidMove 5FP" $ do
        it "simple valid move" $ do
            let b = [[Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]]]
                in isValidMove b (Move (Pos 'a' 6) (Pos 'a' 4) 2) `shouldBe` (True :: Bool)
        it "valid move blue without too tall stack" $ do
            let b = [[Stack [Blue,Red,Red],Empty,Empty,Empty,Empty,Stack [Red,Red,Red]], [Empty,Stack [Red],Empty,Empty,Empty,Empty], [Empty,Empty,Stack [Blue,Blue],Empty,Stack [Blue,Red,Red],Empty], [Empty,Empty,Empty,Empty,Empty,Empty], [Stack [Blue,Red,Red],Stack [Blue,Red,Red],Empty,Empty,Empty,Empty], [Empty,Stack [Blue],Empty,Empty,Stack [Blue,Blue,Blue],Stack [Blue,Blue]]]
                in isValidMove b ((Move (Pos 'e' 4) (Pos 'c' 4) 2)) `shouldBe` (True :: Bool)
        it "valid move red without too tall stack" $ do
            let b = [[Stack [Blue,Red,Red],Stack [Red,Blue,Blue],Empty,Stack [Red,Red],Empty,Empty], [Empty,Empty,Empty,Empty,Stack [Blue,Red,Red],Empty], [Empty,Stack [Blue,Blue,Red,Red],Empty,Empty,Empty,Empty], [Empty,Empty,Empty,Empty,Empty,Stack [Blue]], [Stack [Blue,Blue,Red,Red],Empty,Empty,Empty,Empty,Empty], [Empty,Empty,Empty,Empty,Stack [Blue,Red,Blue,Blue],Empty]]
                in isValidMove b ((Move (Pos 'b' 6) (Pos 'b' 5) 1)) `shouldBe` (True :: Bool)
        it "valid move with too tall stack - keep too tall" $ do
            let b = [[Stack [Blue,Blue,Red,Red,Blue,Red,Red],Stack [Blue,Blue],Empty,Stack [Red,Red],Empty,Empty], [Empty,Empty,Stack [Red],Empty,Stack [Blue,Red,Red],Empty], [Empty,Stack [Blue,Blue,Red,Red],Empty,Empty,Empty,Empty], [Empty,Empty,Empty,Empty,Empty,Stack [Blue]], [Empty,Empty,Empty,Empty,Empty,Empty], [Empty,Empty,Empty,Empty,Stack [Blue,Red,Blue,Blue],Empty]]
                in isValidMove b ((Move (Pos 'a' 6) (Pos 'd' 6) 3)) `shouldBe` (True :: Bool)
        it "valid move complete too tall stack" $ do
            let b = [[Empty,Stack [Red],Stack [Red,Blue,Blue],Stack [Blue,Red],Empty,Empty], [Empty,Empty,Stack [Blue,Red,Blue,Blue,Blue,Red],Stack [Blue,Blue],Empty,Empty], [Empty,Empty,Empty,Empty,Stack [Red,Red],Stack [Blue,Red,Red]], [Empty,Stack [Blue],Empty,Empty,Empty,Empty], [Empty,Stack [Red],Empty,Empty,Empty,Stack [Red]], [Empty,Stack [Red],Empty,Stack [Blue],Empty,Empty]]
                in isValidMove b ((Move (Pos 'c' 5) (Pos 'e' 1) 6)) `shouldBe` (True :: Bool)
        it "move complete too tall stack double mirror red" $ do
            let b = [[Stack [Red,Red],Empty,Stack [Red],Stack [Red],Empty,Empty], [Empty,Empty,Empty,Empty,Stack [Red],Empty], [Empty,Empty,Empty,Empty,Empty,Empty], [Empty,Stack [Red,Red,Red,Red,Blue,Red,Red,Red],Empty,Empty,Stack [Blue,Blue],Empty], [Empty,Stack [Blue],Stack [Blue,Blue,Blue],Empty,Empty,Empty], [Stack [Blue],Empty,Stack [Blue],Stack [Blue],Empty,Stack [Blue,Blue]]]
                in isValidMove b ((Move (Pos 'b' 3) (Pos 'b' 1) 8)) `shouldBe` (True :: Bool)
        it "invalid move with too tall stack - keep stack > 4 blue" $ do
            let b = [[Stack [Blue,Blue,Red,Red,Blue,Red,Red],Stack [Blue,Blue],Empty,Stack [Red,Red],Empty,Empty], [Empty,Empty,Stack [Red],Empty,Stack [Blue,Red,Red],Empty], [Empty,Stack [Blue,Blue,Red,Red],Empty,Empty,Empty,Empty], [Empty,Empty,Empty,Empty,Empty,Stack [Blue]], [Empty,Empty,Empty,Empty,Empty,Empty], [Empty,Empty,Empty,Empty,Stack [Blue,Red,Blue,Blue],Empty]]
                in isValidMove b ((Move (Pos 'a' 6) (Pos 'b' 5) 1)) `shouldBe` (False :: Bool)
        it "invalid move with too tall stack - keep stack > 4 red" $ do
            let b = [[Stack [Red,Red],Empty,Stack [Red],Stack [Red],Empty,Empty], [Empty,Empty,Empty,Empty,Stack [Red],Empty], [Empty,Empty,Empty,Empty,Empty,Empty], [Empty,Stack [Red,Red,Red,Red,Blue,Red,Red,Red],Empty,Empty,Stack [Blue,Blue],Empty], [Empty,Stack [Blue],Stack [Blue,Blue,Blue],Empty,Empty,Empty], [Stack [Blue],Empty,Stack [Blue],Stack [Blue],Empty,Stack [Blue,Blue]]]
                in isValidMove b ((Move (Pos 'b' 3) (Pos 'e' 6) 3)) `shouldBe` (False :: Bool)
        it "try move that is not too tall stack red" $ do
            let b = [[Empty,Empty,Empty,Empty,Empty,Empty], [Empty,Stack [Red],Stack [Red,Red,Blue,Blue,Blue],Empty,Empty,Empty], [Empty,Stack [Blue,Red],Empty,Stack [Red],Stack [Red],Empty], [Empty,Stack [Blue,Red,Red,Red],Stack [Blue],Empty,Stack [Red,Blue,Blue],Empty], [Empty,Empty,Empty,Empty,Stack [Blue],Stack [Blue]], [Empty,Stack [Blue,Red,Blue,Red],Empty,Empty,Empty,Empty]]
                in isValidMove b ((Move (Pos 'b' 5) (Pos 'a' 5) 1)) `shouldBe` (False :: Bool)
        it "try move that is not too tall stack blue" $ do
            let b = [[Empty,Empty,Empty,Empty,Empty,Empty], [Empty,Stack [Red],Stack [Red,Blue,Blue,Blue],Empty,Empty,Empty], [Empty,Stack [Red,Blue,Red],Empty,Stack [Red],Stack [Red],Empty], [Empty,Stack [Red],Stack [Blue],Empty,Stack [Blue,Red,Red,Red,Blue,Blue],Empty], [Empty,Empty,Empty,Empty,Stack [Blue],Stack [Blue]], [Empty,Stack [Blue,Red,Blue,Red],Empty,Empty,Empty,Empty]]
                in isValidMove b ((Move (Pos 'b' 1) (Pos 'e' 1) 3)) `shouldBe` (False :: Bool)

testListMoves :: Spec
testListMoves = describe "IF Grade listMoves 2FP" $ do
        it "red cannot move" $ do
            let board = [[Stack [Blue,Blue,Red,Red],Stack [Blue,Blue,Red,Red],Stack [Blue,Blue,Red,Red],Stack [Blue,Blue,Red,Red],Stack [Blue,Blue,Red,Red],Stack [Blue,Blue,Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty]]
                result = []
                in listMoves board Red `shouldMatchList` (result :: [Move])
        
        it "blue cannot move" $ do
            let b = [[Empty,Empty,Empty,Empty,Empty,Stack [Red]], [Stack [Red,Red],Empty,Stack [Red,Blue,Blue],Empty,Stack [Red],Empty], [Empty,Stack [Red],Stack [Red,Blue,Blue,Red,Blue,Blue,Blue],Empty,Empty,Empty], [Empty,Stack [Red,Red,Blue,Blue],Empty,Empty,Stack [Red],Empty], [Empty,Empty,Empty,Empty,Empty,Stack [Red,Blue,Blue,Blue]], [Empty,Empty,Empty,Empty,Empty,Empty]]
                result = []
                in listMoves b Blue `shouldMatchList` (result :: [Move])

        it "all moves red" $ do
            let b = [[Empty,Empty,Empty,Empty,Empty,Stack [Red]], [Empty,Stack [Red],Stack [Red,Blue,Blue,Blue],Empty,Empty,Empty], [Empty,Stack [Red,Blue,Red],Empty,Stack [Red],Empty,Empty], [Empty,Stack [Red],Stack [Blue],Empty,Stack [Blue,Red,Red,Red,Blue,Blue],Empty], [Empty,Empty,Empty,Empty,Stack [Blue],Stack [Blue]], [Empty,Stack [Blue,Red,Blue,Red],Empty,Empty,Empty,Empty]]
                result = [Move (Pos 'b' 3) (Pos 'b' 4) 1,Move (Pos 'b' 3) (Pos 'c' 4) 1,Move (Pos 'b' 3) (Pos 'c' 3) 1,Move (Pos 'b' 3) (Pos 'c' 2) 1,Move (Pos 'b' 3) (Pos 'b' 2) 1,Move (Pos 'b' 3) (Pos 'a' 2) 1,Move (Pos 'b' 3) (Pos 'a' 3) 1,Move (Pos 'b' 3) (Pos 'a' 4) 1,Move (Pos 'b' 4) (Pos 'b' 5) 3,Move (Pos 'b' 4) (Pos 'b' 6) 2,Move (Pos 'b' 4) (Pos 'b' 5) 1,Move (Pos 'b' 4) (Pos 'e' 5) 3,Move (Pos 'b' 4) (Pos 'd' 6) 2,Move (Pos 'b' 4) (Pos 'c' 5) 1,Move (Pos 'b' 4) (Pos 'e' 4) 3,Move (Pos 'b' 4) (Pos 'd' 4) 2,Move (Pos 'b' 4) (Pos 'c' 4) 1,Move (Pos 'b' 4) (Pos 'e' 1) 3,Move (Pos 'b' 4) (Pos 'd' 2) 2,Move (Pos 'b' 4) (Pos 'c' 3) 1,Move (Pos 'b' 4) (Pos 'b' 1) 3,Move (Pos 'b' 4) (Pos 'b' 2) 2,Move (Pos 'b' 4) (Pos 'b' 3) 1,Move (Pos 'b' 4) (Pos 'c' 1) 3,Move (Pos 'b' 4) (Pos 'a' 3) 1,Move (Pos 'b' 4) (Pos 'c' 4) 3,Move (Pos 'b' 4) (Pos 'a' 4) 1,Move (Pos 'b' 4) (Pos 'c' 5) 3,Move (Pos 'b' 4) (Pos 'a' 5) 1,Move (Pos 'b' 5) (Pos 'b' 6) 1,Move (Pos 'b' 5) (Pos 'c' 6) 1,Move (Pos 'b' 5) (Pos 'c' 5) 1,Move (Pos 'b' 5) (Pos 'c' 4) 1,Move (Pos 'b' 5) (Pos 'b' 4) 1,Move (Pos 'b' 5) (Pos 'a' 4) 1,Move (Pos 'b' 5) (Pos 'a' 5) 1,Move (Pos 'b' 5) (Pos 'a' 6) 1,Move (Pos 'c' 5) (Pos 'c' 3) 4,Move (Pos 'c' 5) (Pos 'c' 4) 3,Move (Pos 'c' 5) (Pos 'c' 6) 1,Move (Pos 'c' 5) (Pos 'e' 3) 4,Move (Pos 'c' 5) (Pos 'f' 4) 3,Move (Pos 'c' 5) (Pos 'e' 5) 2,Move (Pos 'c' 5) (Pos 'd' 6) 1,Move (Pos 'c' 5) (Pos 'e' 5) 4,Move (Pos 'c' 5) (Pos 'f' 5) 3,Move (Pos 'c' 5) (Pos 'd' 5) 1,Move (Pos 'c' 5) (Pos 'e' 1) 4,Move (Pos 'c' 5) (Pos 'f' 2) 3,Move (Pos 'c' 5) (Pos 'e' 3) 2,Move (Pos 'c' 5) (Pos 'd' 4) 1,Move (Pos 'c' 5) (Pos 'c' 1) 4,Move (Pos 'c' 5) (Pos 'c' 2) 3,Move (Pos 'c' 5) (Pos 'c' 3) 2,Move (Pos 'c' 5) (Pos 'c' 4) 1,Move (Pos 'c' 5) (Pos 'b' 2) 3,Move (Pos 'c' 5) (Pos 'a' 3) 2,Move (Pos 'c' 5) (Pos 'b' 4) 1,Move (Pos 'c' 5) (Pos 'b' 5) 3,Move (Pos 'c' 5) (Pos 'a' 5) 2,Move (Pos 'c' 5) (Pos 'b' 5) 1,Move (Pos 'c' 5) (Pos 'b' 4) 3,Move (Pos 'c' 5) (Pos 'b' 6) 1,Move (Pos 'd' 4) (Pos 'd' 5) 1,Move (Pos 'd' 4) (Pos 'e' 5) 1,Move (Pos 'd' 4) (Pos 'e' 4) 1,Move (Pos 'd' 4) (Pos 'e' 3) 1,Move (Pos 'd' 4) (Pos 'd' 3) 1,Move (Pos 'd' 4) (Pos 'c' 3) 1,Move (Pos 'd' 4) (Pos 'c' 4) 1,Move (Pos 'd' 4) (Pos 'c' 5) 1,Move (Pos 'f' 6) (Pos 'f' 5) 1,Move (Pos 'f' 6) (Pos 'e' 5) 1,Move (Pos 'f' 6) (Pos 'e' 6) 1]
                in listMoves b Red `shouldMatchList` (result :: [Move])
        it "all moves blue" $ do
            let b = [[Empty,Empty,Empty,Empty,Empty,Stack [Red]], [Stack [Red,Red],Empty,Stack [Blue,Blue],Stack [Red],Stack [Red],Empty], [Empty,Stack [Red,Red],Stack [Red,Blue,Blue,Blue],Stack [Red,Red],Empty,Empty], [Empty,Stack [Red,Red,Blue,Blue],Empty,Empty,Empty,Empty], [Stack [Blue,Blue],Empty,Empty,Empty,Empty,Stack [Blue,Blue]], [Empty,Empty,Empty,Empty,Stack [Blue],Empty]]
                result = [Move (Pos 'a' 2) (Pos 'a' 4) 2,Move (Pos 'a' 2) (Pos 'a' 3) 1,Move (Pos 'a' 2) (Pos 'c' 4) 2,Move (Pos 'a' 2) (Pos 'b' 3) 1,Move (Pos 'a' 2) (Pos 'c' 2) 2,Move (Pos 'a' 2) (Pos 'b' 2) 1,Move (Pos 'a' 2) (Pos 'b' 1) 1,Move (Pos 'a' 2) (Pos 'a' 1) 1,Move (Pos 'c' 5) (Pos 'c' 6) 1,Move (Pos 'c' 5) (Pos 'e' 5) 2,Move (Pos 'c' 5) (Pos 'd' 6) 1,Move (Pos 'c' 5) (Pos 'd' 5) 1,Move (Pos 'c' 5) (Pos 'e' 3) 2,Move (Pos 'c' 5) (Pos 'd' 4) 1,Move (Pos 'c' 5) (Pos 'c' 3) 2,Move (Pos 'c' 5) (Pos 'c' 4) 1,Move (Pos 'c' 5) (Pos 'a' 3) 2,Move (Pos 'c' 5) (Pos 'b' 4) 1,Move (Pos 'c' 5) (Pos 'a' 5) 2,Move (Pos 'c' 5) (Pos 'b' 5) 1,Move (Pos 'c' 5) (Pos 'b' 6) 1,Move (Pos 'e' 1) (Pos 'e' 2) 1,Move (Pos 'e' 1) (Pos 'f' 2) 1,Move (Pos 'e' 1) (Pos 'f' 1) 1,Move (Pos 'e' 1) (Pos 'd' 2) 1,Move (Pos 'e' 1) (Pos 'd' 1) 1,Move (Pos 'f' 2) (Pos 'f' 4) 2,Move (Pos 'f' 2) (Pos 'f' 3) 1,Move (Pos 'f' 2) (Pos 'd' 4) 2,Move (Pos 'f' 2) (Pos 'e' 3) 1,Move (Pos 'f' 2) (Pos 'd' 2) 2,Move (Pos 'f' 2) (Pos 'e' 2) 1,Move (Pos 'f' 2) (Pos 'e' 1) 1,Move (Pos 'f' 2) (Pos 'f' 1) 1]
                in listMoves b Blue `shouldMatchList` (result :: [Move])
        it "start board red" $ do
            let b = [[Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]], [Empty,Empty,Empty,Empty,Empty,Empty], [Empty,Empty,Empty,Empty,Empty,Empty], [Empty,Empty,Empty,Empty,Empty,Empty], [Empty,Empty,Empty,Empty,Empty,Empty], [Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]]]
                result = [Move (Pos 'a' 6) (Pos 'a' 4) 2,Move (Pos 'a' 6) (Pos 'a' 5) 1,Move (Pos 'a' 6) (Pos 'c' 4) 2,Move (Pos 'a' 6) (Pos 'b' 5) 1,Move (Pos 'a' 6) (Pos 'c' 6) 2,Move (Pos 'a' 6) (Pos 'b' 6) 1,Move (Pos 'b' 6) (Pos 'b' 4) 2,Move (Pos 'b' 6) (Pos 'b' 5) 1,Move (Pos 'b' 6) (Pos 'd' 4) 2,Move (Pos 'b' 6) (Pos 'c' 5) 1,Move (Pos 'b' 6) (Pos 'd' 6) 2,Move (Pos 'b' 6) (Pos 'c' 6) 1,Move (Pos 'b' 6) (Pos 'a' 5) 1,Move (Pos 'b' 6) (Pos 'a' 6) 1,Move (Pos 'c' 6) (Pos 'c' 4) 2,Move (Pos 'c' 6) (Pos 'c' 5) 1,Move (Pos 'c' 6) (Pos 'e' 4) 2,Move (Pos 'c' 6) (Pos 'd' 5) 1,Move (Pos 'c' 6) (Pos 'e' 6) 2,Move (Pos 'c' 6) (Pos 'd' 6) 1,Move (Pos 'c' 6) (Pos 'a' 4) 2,Move (Pos 'c' 6) (Pos 'b' 5) 1,Move (Pos 'c' 6) (Pos 'a' 6) 2,Move (Pos 'c' 6) (Pos 'b' 6) 1,Move (Pos 'd' 6) (Pos 'd' 4) 2,Move (Pos 'd' 6) (Pos 'd' 5) 1,Move (Pos 'd' 6) (Pos 'f' 4) 2,Move (Pos 'd' 6) (Pos 'e' 5) 1,Move (Pos 'd' 6) (Pos 'f' 6) 2,Move (Pos 'd' 6) (Pos 'e' 6) 1,Move (Pos 'd' 6) (Pos 'b' 4) 2,Move (Pos 'd' 6) (Pos 'c' 5) 1,Move (Pos 'd' 6) (Pos 'b' 6) 2,Move (Pos 'd' 6) (Pos 'c' 6) 1,Move (Pos 'e' 6) (Pos 'e' 4) 2,Move (Pos 'e' 6) (Pos 'e' 5) 1,Move (Pos 'e' 6) (Pos 'f' 5) 1,Move (Pos 'e' 6) (Pos 'f' 6) 1,Move (Pos 'e' 6) (Pos 'c' 4) 2,Move (Pos 'e' 6) (Pos 'd' 5) 1,Move (Pos 'e' 6) (Pos 'c' 6) 2,Move (Pos 'e' 6) (Pos 'd' 6) 1,Move (Pos 'f' 6) (Pos 'f' 4) 2,Move (Pos 'f' 6) (Pos 'f' 5) 1,Move (Pos 'f' 6) (Pos 'd' 4) 2,Move (Pos 'f' 6) (Pos 'e' 5) 1,Move (Pos 'f' 6) (Pos 'd' 6) 2,Move (Pos 'f' 6) (Pos 'e' 6) 1]
                in listMoves b Red `shouldMatchList` (result :: [Move])