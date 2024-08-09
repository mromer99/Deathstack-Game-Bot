# Deathstacks Game 

## Overview

This homework involves implementing and validating a board game called Deathstacks. The primary tasks include implementing game mechanics, rules, and validations, as well as generating possible moves for players. This documentation covers all the components and functions developed as part of the assignment.

## Table of Contents

- [Game Overview](#game-overview)
- [Data Structures](#data-structures)
  - [Player](#player)
  - [Cell](#cell)
  - [Pos](#pos)
  - [Dir](#dir)
  - [Move](#move)
- [Core Functions](#core-functions)
  - [Path Function](#path-function)
  - [Possible Moves](#possible-moves)
  - [Is Valid Move](#is-valid-move)
  - [List Moves](#list-moves)
  - [Player Won](#player-won)
- [Game Rules](#game-rules)
  - [Too-Tall Rule](#too-tall-rule)
  - [Reflection Rules](#reflection-rules)
- [Example Usage](#example-usage)
- [License](#license)

## Game Overview

Deathstacks is a strategic board game where players move stacks of pieces across a 6x6 grid. The objective is to be the last player with pieces remaining on the board. The game includes various rules to regulate the movement of stacks, particularly when they become too tall.

## Data Structures

### Player
```haskell
data Player = Red | Blue deriving (Show, Eq)
```
- **Description**: Represents the two players in the game, Red and Blue.
### Cell
```haskell
data Cell = Stack [Player] | Empty deriving (Show, Eq)
```
- **Description**: Represents a cell on the board. A cell can either be empty or contain a stack of pieces belonging to one or both players.
### Pos
```haskell
data Pos = Pos { col :: Char, row :: Int } deriving (Show, Eq)
```
- **Description**: Represents a position on the board, identified by a column (a character between 'a' and 'f') and a row (an integer between 1 and 6).
### Dir
```haskell
data Dir = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest deriving (Show, Eq)
```
- **Description**: Represents the eight possible directions in which a stack can be moved on the board.
### Move
```haskell
data Move = Move { start :: Pos, target :: Pos, steps :: Int } deriving (Show, Eq)
```
- **Description**: Represents a move in the game, including the starting position, target position, and the number of steps taken.

## Core Functions

### Path Function
```haskell
path :: Pos -> Dir -> Int -> [Pos]
```
Calculates the path of a move, considering the board's boundaries and reflecting the direction if necessary.
### Possible Moves
```haskell
path :: Pos -> Dir -> Int -> [Pos]
```
Calculates the path of a move, considering the board's boundaries and reflecting the direction if necessary.
### Is Valid Move
```haskell
isValidMove :: Board -> Move -> Bool
```
Calculates the path of a move, considering the board's boundaries and reflecting the direction if necessary.
### List Moves
```haskell
listMoves :: Board -> Player -> [Move]
```
Calculates the path of a move, considering the board's boundaries and reflecting the direction if necessary.
### Player Won
```haskell
playerWon :: Board -> Maybe Player
```
Calculates the path of a move, considering the board's boundaries and reflecting the direction if necessary.

## Game Rules
### Too Tall Rule
If a stack has more than 4 pieces, the player must move enough pieces to reduce the stack to 4 or fewer.
### Reflection Rules
Moves that would take a stack off the board reflect back in the opposite direction.

## Example Usage
```Haskell
-- Define a board state
let board = [[Empty, Stack [Red], Empty], 
             [Empty, Stack [Blue, Red, Blue], Stack [Red]], 
             [Stack [Red, Red], Empty, Empty]]

-- Get all valid moves for Red
let moves = listMoves board Red

-- Check if a specific move is valid
let validMove = isValidMove board (Move (Pos 'b' 2) (Pos 'c' 3) 2)
```
## License
This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.







