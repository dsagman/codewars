-- Othello in Haskell
-- With much help from:
-- Peter Norvig's Paradigms of Artificial Intelligence Programming, 1992
-- https://github.com/norvig/paip-lisp/blob/main/docs/chapter18.md
-- StackOverflow, copilot, chatGPT

module Main where
import Data.List.Extra


data Cell = Empty | Black | White | Wall
  deriving (Eq)

data Player = BlackP | WhiteP
  deriving (Eq, Show)

instance Show Cell where
    show :: Cell -> String
    show Empty = "·"
    show Black = "●"
    show White = "○"
    show Wall  = "█"

type Board = [Cell]

initialBoard :: Board
initialBoard =
    replicate 10 Wall ++
    take 30 (cycle (Wall : replicate 8 Empty ++ [Wall])) ++
    (Wall : replicate 3 Empty ++ [White, Black] ++ replicate 3 Empty ++ [Wall]) ++
    (Wall : replicate 3 Empty ++ [Black, White] ++ replicate 3 Empty ++ [Wall]) ++
    take 30 (cycle (Wall : replicate 8 Empty ++ [Wall])) ++
    replicate 10 Wall

showBoard :: Board -> IO ()
showBoard board = do
    putStrLn ""
    mapM_ putStrLn (zipWith showRow [0..] (chunksOf 10 board)) 
    let blackCount = length (filter (== Black) board)
    let whiteCount = length (filter (== White) board)
    putStrLn $ "  Black: " ++ show blackCount ++ " White: " ++ show whiteCount
    putStrLn ""
  where
    showRow rowIndex xs
        | all (== Wall) xs = "  " ++ unwords (map show [1..8])  -- Column numbers
        | otherwise = show rowIndex ++ " " ++ unwords (map show (init (tail xs))) ++ " " ++ show rowIndex -- Row number


directions :: [Int]
directions = [-11, -10, -9, -1, 1, 9, 10, 11]

-- The board and walls

-- 0  1  2  3  4  5  6  7  8  9
--10 11 12 13 14 15 16 17 18 19
--20 21 22 23 24 25 26 27 28 29
--30 31 32 33 34 35 36 37 38 39
--40 41 42 43 44 45 46 47 48 49
--50 51 52 53 54 55 56 57 58 59
--60 61 62 63 64 65 66 67 68 69
--70 71 72 73 74 75 76 77 78 79
--80 81 82 83 84 85 86 87 88 89
--90 91 92 93 94 95 96 97 98 99

validP :: [Int]
-- Valid moves are numbers in the range 11-88 that end in 1-8.
validP = [move | move <- [11..88], (move `mod` 10) `elem` [1..8] ]

-- legalP :: Player -> Board -> [Int]
-- A Legal move must be into an empty square, and it must
-- flip at least one opponent piece.
legalP :: Player -> Board -> [Int]
legalP player board = validP
    where validP = map fst $ filter ((== Empty) . snd) $ zip [1..99] board

makeMove :: Player -> Int -> Board -> Board
-- Assumes the move is legal
makeMove player move board = 
    case player of
        BlackP -> before ++ Black : after
        WhiteP -> before ++ White : after
    where (before, _ : after) = splitAt move board
           


main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    showBoard initialBoard
    putStrLn "So much left to do"

