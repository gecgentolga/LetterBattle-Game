-- GameActions.hs
-- This module determines the valid moves on the game board.

module GameActions (setAvailableMoves, isValidPosition) where

import Data.Char (toUpper)

-- The board is represented as a list of 15 cells
type Board = [Char]
type Position = Int

-- Valid positions
validPositions :: [Position]
validPositions = [1, 2, 3, 5, 6, 7, 8, 9, 11, 12, 13]

-- Checks if the position is valid and empty ("_")
isValidPosition :: Board -> Position -> Bool
isValidPosition board pos = pos `elem` validPositions && board !! pos == '_'

-- Defines move directions
zDirections, abcDirections :: [Int]
zDirections = [-5, 5, -1, 1, -4, -6, 6, 4]
abcDirections = [1, 5,-5, -4, 6, 4]

-- Calculates the positions in which a letter can move for a given letter.
-- The `type` parameter can be "z" or "abc"
setAvailableMoves :: String -> Char -> Board -> [Position]
setAvailableMoves "z" letter board =
    let index = findIndex letter board
    in if index == 9
       then [ index + d | d <- zDirections, d /= -4,
                          let newPos = index + d,
                          newPos >= 0, newPos < 15,
                          isValidPosition board newPos ]
       else [ index + d | d <- zDirections,
                          let newPos = index + d,
                          newPos >= 0, newPos < 15,
                          isValidPosition board newPos ]
setAvailableMoves "abc" letter board =
    let index = findIndex letter board
    in [ index + d | d <- abcDirections,
                     let newPos = index + d,
                     newPos >= 0, newPos < 15,
                     isValidPosition board newPos ]
setAvailableMoves _ _ _ = []

-- Finds the index of the letter on the board
findIndex :: Char -> Board -> Int
findIndex ch board =
    case lookup ch (zip board [0..]) of
        Just i  -> i
        Nothing -> error $ "Character " ++ [ch] ++ " not found on board"
