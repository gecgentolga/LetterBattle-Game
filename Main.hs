-- Main.hs
-- printBoard function from GameView to print the board
import GameView (printBoard)
import Algorithms (playGame1, playGame2)
import System.IO (hFlush, stdout)
import Data.Char (toLower)

-- Initial state of the game board
-- 'x' blocks in fixed locations
-- The positions of A, B, C and Z are as follows
initialBoard :: [Char]
initialBoard = ['x', 'A', '_', '_', 'x',
                'B', '_', '_', '_', 'Z',
                'x', 'C', '_', '_', 'x']

-- Get game type and maximum number of moves from user
getGameSettings :: IO (String, Int)
getGameSettings = do
    putStrLn "Welcome!"
    printBoard initialBoard
    putStr "Enter the maximum number of total moves allowed: "
    hFlush stdout
    movesInput <- getLine
    let maxMoves = case reads movesInput of
                        [(n, "")] -> n
                        _         -> 20

    gameType <- promptGameType
    return (gameType, maxMoves)

-- Get the choice of 'firsts' or 'last'
promptGameType :: IO String
promptGameType = do
    putStr "Who starts first? Type 'firsts' or 'last': "
    hFlush stdout
    input <- getLine
    case map toLower input of
        "firsts" -> return "abc"
        "last"   -> return "z"
        _        -> putStrLn "Invalid choice. Try again." >> promptGameType



main :: IO ()
main = do
    (gameType, maxMoves) <- getGameSettings
    case gameType of
        "abc" -> playGame1 maxMoves initialBoard
        "z"   -> playGame2 maxMoves initialBoard
        _     -> putStrLn "Error: Invalid game type."
