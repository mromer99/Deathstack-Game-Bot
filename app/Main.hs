module Main (main) where

import System.Random
import System.Environment

import Deathstacks (playerWon, isValidMove, listMoves, Move (Move))
import Board (validateFEN, buildBoard, Player (Red, Blue), Pos (Pos))


main :: IO ()
main = do
    args <- getArgs
    let (fen:p:_) = args
        player = if p == "r" then Red else Blue 
        moves = listMoves (buildBoard fen) player in
            if (last args) == "-all" then
                putStrLn (show moves)
            else if (last args) == "-validateFEN" then
                putStrLn (show (validateFEN fen))
            else if (last args) == "-buildBoard" then
                putStrLn (show (buildBoard fen))
            else if (last args) == "-playerWon" then
                putStrLn (show (playerWon (buildBoard fen)))
            else if (last args) == "-isValidMove" then
                putStrLn (show ((isValidMove (buildBoard fen)) (buildMove p)))
            else
                do
                    rand <- randomRIO (0, (length moves)-1)
                    putStrLn (show (moves!!rand)) 

buildMove :: String -> Move
buildMove (ax:ay:bx:by:steps) = Move (Pos ax (read [ay])) (Pos bx (read [by])) (read steps)