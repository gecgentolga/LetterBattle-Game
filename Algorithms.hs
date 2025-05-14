-- Algorithms.hs
-- Module containing the main logic of the game

module Algorithms (playGame1, playGame2) where

import GameActions (setAvailableMoves)
import GameView (printBoard)
import Control.Monad (when)
import Data.Char (toUpper)
import System.IO (hFlush, stdout)

type Board = [Char]

-- Finds the index of the letter on the board
findIndex :: Char -> Board -> Int
findIndex ch board =
    case lookup ch (zip board [0..]) of
        Just i  -> i
        Nothing -> 15  -- Harf yoksa oyun dışında demektir

-- The process of moving a letter to a given position
moveLetter :: Board -> Char -> Int -> IO (Board, Bool)
moveLetter board letter toPos = do
    let pieceType = if letter `elem` "ABC" then "abc" else "z"
    let validMoves = setAvailableMoves pieceType letter board
    if toPos `elem` validMoves
        then do
            let oldIndex = findIndex letter board
            let newBoard = replaceNth oldIndex '_' (replaceNth toPos letter board)
            return (newBoard, True)
        else do
            putStrLn "invalid move"
            return (board, False)

-- Helper: Replaces the value at the specified index of the list
replaceNth :: Int -> a -> [a] -> [a]
replaceNth n newVal xs = take n xs ++ [newVal] ++ drop (n + 1) xs

-- Column calculation from index (columns 0-4)
calculateColumn :: Int -> Int
calculateColumn x
    | x >= 0 && x <= 4   = x
    | x >= 5 && x <= 9   = x - 5
    | x >= 10 && x <= 14 = x - 10
    | otherwise          = 5

-- Z's win status check
checkZWin :: Board -> Bool
checkZWin board =
    let zCol = calculateColumn (findIndex 'Z' board)
        aCol = calculateColumn (findIndex 'A' board)
        bCol = calculateColumn (findIndex 'B' board)
        cCol = calculateColumn (findIndex 'C' board)
    in zCol < minimum [aCol, bCol, cCol]

-- Check A/B/C win status
checkABCWin :: Board -> Bool
checkABCWin board =
    null $ setAvailableMoves "z" 'Z' board

-- Takes letter and position input from the user
getMoveInput :: IO (Maybe (Char, Int))
getMoveInput = do
    putStr "Please select one of the first three letters and a cell to move it (e.g., A 6): "
    hFlush stdout
    line <- getLine
    let parts = words $ map toUpper line
    case parts of
        [l, pStr] | l `elem` ["A", "B", "C"] ->
            case reads pStr :: [(Int, String)] of
                [(pos, "")] -> return $ Just (head l, pos)
                _           -> return Nothing
        _ -> return Nothing

-- Takes position input for Z from the user
getZMoveInput :: IO (Maybe Int)
getZMoveInput = do
    putStr "Please select a cell for the Z: "
    hFlush stdout
    input <- getLine
    case reads input of
        [(pos, "")] -> return $ Just pos
        _           -> return Nothing

-- Play order: first play A/B/C, then play Z
playGame1 :: Int -> Board -> IO ()
playGame1 maxMoves board = gameLoop board 0 True
  where
    gameLoop b moves isABC
        | moves >= maxMoves = putStrLn "Maximum moves reached. It's a draw!" >> printBoard b
        | otherwise = do
            printBoard b
            if isABC
                then do
                    move <- getMoveInput
                    case move of
                        Just (letter, pos) -> do
                            (newBoard, _) <- moveLetter b letter pos
                            gameLoop newBoard (moves + 1) False
                        Nothing -> putStrLn "invalid move" >> gameLoop b moves isABC
                else do
                    let zOptions = setAvailableMoves "z" 'Z' b
                    if null zOptions
                        then putStrLn "   A&B&C wins!" >> printBoard b
                        else do
                            move <- getZMoveInput
                            case move of
                                Just pos -> do
                                    (newBoard, _) <- moveLetter b 'Z' pos
                                    printBoard newBoard
                                    if checkZWin newBoard
                                        then putStrLn "   Z wins!" >> printBoard newBoard
                                        else if checkABCWin newBoard
                                            then putStrLn "   A&B&C wins!" >> printBoard newBoard
                                            else gameLoop newBoard (moves + 1) True
                                Nothing -> putStrLn "invalid move" >> gameLoop b moves isABC

-- Play order: play Z first, then A/B/C
playGame2 :: Int -> Board -> IO ()
playGame2 maxMoves board = gameLoop board 0 True
  where
    gameLoop b moves isZ
        | moves >= maxMoves = putStrLn "Maximum moves reached. It's a draw!" >> printBoard b
        | otherwise = do
            printBoard b
            if isZ
                then do
                    let zOptions = setAvailableMoves "z" 'Z' b
                    if null zOptions
                        then putStrLn "   A&B&C wins!" >> printBoard b
                        else do
                            move <- getZMoveInput
                            case move of
                                Just pos -> do
                                    (newBoard, _) <- moveLetter b 'Z' pos
                                    printBoard newBoard
                                    if checkZWin newBoard
                                        then putStrLn "   Z wins!" >> printBoard newBoard
                                        else if checkABCWin newBoard
                                            then putStrLn "   A&B&C wins!" >> printBoard newBoard
                                            else gameLoop newBoard (moves + 1) False
                                Nothing -> putStrLn "invalid move" >> gameLoop b moves isZ
                else do
                    move <- getMoveInput
                    case move of
                        Just (letter, pos) -> do
                            (newBoard, _) <- moveLetter b letter pos
                            printBoard newBoard
                            if checkZWin newBoard
                                then putStrLn "   Z wins!" >> printBoard newBoard
                                else if checkABCWin newBoard
                                    then putStrLn "   A&B&C wins!" >> printBoard newBoard
                                    else gameLoop newBoard (moves + 1) True
                        Nothing -> putStrLn "invalid move" >> gameLoop b moves isZ
