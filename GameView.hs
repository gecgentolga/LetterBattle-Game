-- GameView.hs
-- This module prints the game board to the terminal.

module GameView (printBoard) where

import Data.List (intercalate)
import Control.Monad (forM_)

type Board = [Char]

-- Prints the board to the terminal in 5x3 format
printBoard :: Board -> IO ()
printBoard board = do
    --Print the board in steps of 5 from 0 to 15
    let rows = [ take 5 $ drop i board | i <- [0,5,10] ]
    forM_ rows $ \row -> do
        putStrLn $ intercalate "\t" (map (:[]) row)
    putStrLn ""  -- Extra line spacing
