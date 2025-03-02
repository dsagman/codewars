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
type BoardIdx = [(Int, Cell)]

data Neighbors = Neighbors
  { left :: BoardIdx
    , right :: BoardIdx
    , up :: BoardIdx
    , down :: BoardIdx
    , diagNE :: BoardIdx
    , diagNW :: BoardIdx
    , diagSE :: BoardIdx
    , diagSW :: BoardIdx
  } deriving (Show)

boardN :: Int
boardN = 8

boardIdx :: Board -> BoardIdx
boardIdx = zip [0..99]

-- With walls for out of bounds. Not sure we need this.
-- initialBoard :: Board
-- initialBoard =
--     replicate 10 Wall ++
--     take 30 (cycle (Wall : replicate 8 Empty ++ [Wall])) ++
--     (Wall : replicate 3 Empty ++ [White, Black] ++ replicate 3 Empty ++ [Wall]) ++
--     (Wall : replicate 3 Empty ++ [Black, White] ++ replicate 3 Empty ++ [Wall]) ++
--     take 30 (cycle (Wall : replicate 8 Empty ++ [Wall])) ++
--     replicate 10 Wall

initialBoard :: Board
initialBoard =
    take (boardN*((boardN `div` 2)-1)) (cycle (replicate boardN Empty)) ++
    (replicate ((boardN `div` 2)-1) Empty ++ [White, Black] ++ replicate ((boardN `div` 2)-1) Empty) ++
    (replicate ((boardN `div` 2)-1) Empty ++ [Black, White] ++ replicate ((boardN `div` 2)-1) Empty) ++
    take (boardN*((boardN `div` 2)-1)) (cycle (replicate boardN Empty))

showBoard :: Board -> IO ()
showBoard board = do
    putStrLn ""
    putStrLn "  a b c d e f g h"
    mapM_ putStrLn (zipWith showRow [0..] (chunksOf boardN board))
    putStrLn "  a b c d e f g h"
    let blackCount = length (filter (== Black) board)
    let whiteCount = length (filter (== White) board)
    putStrLn $ "  Black: " ++ show blackCount ++ " White: " ++ show whiteCount
    putStrLn ""
  where
    showRow rowIndex xs
        | all (== Wall) xs = "  "
        | otherwise = show rowIndex ++ " " ++ unwords (map show (init (tail xs))) ++ " " ++ show rowIndex -- Row number

printNeighbors :: Int -> Board -> IO ()
printNeighbors idx board = do
    let n = neighbors idx board
    putStrLn $ "Left: " ++ show (left n)
    putStrLn $ "Right: " ++ show (right n)
    putStrLn $ "Up: " ++ show (up n)
    putStrLn $ "Down: " ++ show (down n)
    putStrLn $ "DiagNE: " ++ show (diagNE n)
    putStrLn $ "DiagNW: " ++ show (diagNW n)
    putStrLn $ "DiagSE: " ++ show (diagSE n)
    putStrLn $ "DiagSW: " ++ show (diagSW n)

allNeighbors :: Board -> [Neighbors]
allNeighbors board = map (`neighbors` board) [0..99]

neighbors :: Int -> Board -> Neighbors
neighbors idx board =
    let bidx = boardIdx board
    in foldr categorize (Neighbors [] [] [] [] [] [] [] []) bidx
  where
    row = idx `div` boardN
    col = idx `mod` boardN
    categorize (b, x) ns
        | rowB == row && b < idx               = ns { left = (b, x) : left ns }
        | rowB == row && b > idx               = ns { right = (b, x) : right ns }
        | colB == col && b < idx               = ns { up = (b, x) : up ns }
        | colB == col && b > idx               = ns { down = (b, x) : down ns }
        | nw_se == 0  && b < idx && colB < col = ns { diagNW = (b, x) : diagNW ns }
        | nw_se == 0  && b > idx && colB > col = ns { diagSE = (b, x) : diagSE ns }
        | ne_sw == 0  && b < idx && colB > col = ns { diagNE = (b, x) : diagNE ns }
        | ne_sw == 0  && b > idx && colB < col = ns { diagSW = (b, x) : diagSW ns }
        | otherwise                            = ns
        where rowB = b `div` boardN
              colB = b `mod` boardN
              nw_se = (b - idx) `mod` (boardN + 1)
              ne_sw = (b - idx) `mod` (boardN - 1)

-- 0  1  2  3  4  5  6  7   
-- 8  9 10 11 12 13 14 15
--16 17 18 19 20 21 22 23
--24 25 26 27 28 29 30 31
--32 33 34 35 36 37 38 39
--40 41 42 43 44 45 46 47
--48 49 50 51 52 53 54 55
--56 57 58 59 60 61 62 63

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

legalP :: Player -> Board -> [Int]
-- A Legal move must be into an empty square, and it must
-- flip at least one opponent piece.
legalP player board = validP
    where validP = map fst $ filter ((== Empty) . snd) $ boardIdx board

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

