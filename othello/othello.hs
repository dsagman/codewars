---------------------------------------------
--
-- Othello in Haskell
-- David Silverman github.com/dsagman
-- With much help from:
-- Peter Norvig's Paradigms of Artificial Intelligence Programming, 1992
-- https://github.com/norvig/paip-lisp/blob/main/docs/chapter18.md
-- StackOverflow, copilot, chatGPT
--
---------------------------------------------

module Main where
import Data.List.Extra ( nub, chunksOf )
import Data.Maybe ( mapMaybe )

---------------------------------------------
--
--          Data Types
--
---------------------------------------------
data Cell = Empty | Black | White
  deriving (Eq)

data Player = BlackP | WhiteP
  deriving (Eq, Show)

instance Show Cell where
    show :: Cell -> String
    show Empty = "·"
    show Black = "○"
    show White = "●"

type Board = [Cell]
type BoardIdx = [(Int, Cell)]

data Neighbors = Neighbors
  { left     :: BoardIdx
    , right  :: BoardIdx
    , up     :: BoardIdx
    , down   :: BoardIdx
    , diagNE :: BoardIdx
    , diagNW :: BoardIdx
    , diagSE :: BoardIdx
    , diagSW :: BoardIdx
  } deriving (Show)

data Score = Score {blackS :: Int, whiteS :: Int}
  deriving (Show)

---------------------------------------------
--
--          Board setup and indexing
--
---------------------------------------------

-- The board is a single linked list from 0 to 63
-- Algebraic notation (ala chess) is used to identify the cells
-- e.g., a1 is 0, h8 is 63

--           a  b  c  d  e  f  g  h
--          -----------------------
--      1 |  0  1  2  3  4  5  6  7   
--      2 |  8  9 10 11 12 13 14 15
--      3 | 16 17 18 19 20 21 22 23
--      4 | 24 25 26 27 28 29 30 31
--      5 | 32 33 34 35 36 37 38 39
--      6 | 40 41 42 43 44 45 46 47
--      7 | 48 49 50 51 52 53 54 55
--      8 | 56 57 58 59 60 61 62 63

boardN :: Int
boardN = 8 

boardIdx :: Board -> BoardIdx
boardIdx = zip [0..boardN*boardN-1]

idxToAlg :: Int -> String
idxToAlg idx = [toEnum (97 + (idx `mod` boardN)), toEnum (49 + (idx `div` boardN))]

algToIdx :: String -> Int
algToIdx [c1, c2] = (fromEnum c2 - 49) * boardN + (fromEnum c1 - 97)

getFstIdx :: BoardIdx -> Int
getFstIdx = fst . head

initialBoard :: Board
initialBoard =
    take (boardN*((boardN `div` 2)-1)) (cycle (replicate boardN Empty)) ++
    (replicate ((boardN `div` 2)-1) Empty ++ [White, Black] ++ replicate ((boardN `div` 2)-1) Empty) ++
    (replicate ((boardN `div` 2)-1) Empty ++ [Black, White] ++ replicate ((boardN `div` 2)-1) Empty) ++
    take (boardN*((boardN `div` 2)-1)) (cycle (replicate boardN Empty))

---------------------------------------------
--
--          Player utility functions
--
---------------------------------------------

opP :: Player -> Cell
opP BlackP = White
opP WhiteP = Black

isP :: Player -> Cell
isP BlackP = Black
isP WhiteP = White

switchP :: Player -> Player
switchP BlackP = WhiteP
switchP WhiteP = BlackP

---------------------------------------------
--
--          Move finding functions
--
---------------------------------------------

---------------------------------------------
-- neighbors finds all of the pieces in all of directions from a given index
-- O(n) because it makes a single pass through the entire board
--      and fills in all the direction records in one go
---------------------------------------------
neighbors :: Int -> Board -> Neighbors
neighbors idx board =
    let bidx = boardIdx board
    in fixReversed $ foldr categorize (Neighbors [] [] [] [] [] [] [] []) bidx
  where
    row = idx `div` boardN
    col = idx `mod` boardN
    categorize (b, x) ns
        | rowB == row && b < idx               = ns { left   = (b, x) : left   ns }
        | rowB == row && b > idx               = ns { right  = (b, x) : right  ns }
        | colB == col && b < idx               = ns { up     = (b, x) : up     ns }
        | colB == col && b > idx               = ns { down   = (b, x) : down   ns }
        | nw_se == 0  && b < idx && colB < col = ns { diagNW = (b, x) : diagNW ns }
        | nw_se == 0  && b > idx && colB > col = ns { diagSE = (b, x) : diagSE ns }
        | ne_sw == 0  && b < idx && colB > col = ns { diagNE = (b, x) : diagNE ns }
        | ne_sw == 0  && b > idx && colB < col = ns { diagSW = (b, x) : diagSW ns }
        | otherwise                            = ns
        where rowB = b `div` boardN
              colB = b `mod` boardN
              nw_se = (b - idx) `mod` (boardN + 1)
              ne_sw = (b - idx) `mod` (boardN - 1)
    fixReversed ns =
        let blank = (idx, Empty) in
        ns
        { left   = blank : reverse (left ns)
        , up     = blank : reverse (up ns)
        , diagNE = blank : reverse (diagNE ns)
        , diagNW = blank : reverse (diagNW ns)
        , right  = blank : right ns
        , down   = blank : down ns
        , diagSE = blank : diagSE ns
        , diagSW = blank : diagSW ns
        }

---------------------------------------------
-- Only legal moves are on empty cells
-- Returns a list of neighbors for each empty cell
---------------------------------------------
emptyNeighbors :: Board -> [Neighbors]
emptyNeighbors board = map (`neighbors` board) emptyIdx
    where emptyIdx = map fst $ filter ((== Empty) . snd) $ boardIdx board

---------------------------------------------
-- All possible plays for a given player
-- Flippable is if the second element of the list is the opposite of the player,
--            and there is at least one element of the list that is the player
-- Returns a list of tuples where the first element is the index of a valid move
--            and the rest of the list are the cells that can be flipped
-- flipList contains only want the list up to the opponent's piece, 
--            and we don't need the direction anymore
-- Has duplicate keys for different directions, use allDirectionPlays to combine
---------------------------------------------
possiblePlays :: Player -> Board -> [BoardIdx]
possiblePlays player board = do
    let flippable =
            concatMap (filter (any ((== isP player) . snd) . snd) . findStartsWith player) ens
    (dir, ns) <- flippable
    let flipList = takeWhile ((/= isP player) . snd) ns
    pure flipList
    where ens = emptyNeighbors board
    
---------------------------------------------
-- Function to find neighbor lists that start with opponet's piece
-- This means we can flip pieces in that direction
-- Returns both the possible plays and the direction
-- checkStart is a helper function to check if the list starts with an opponent's piece
-- Used in possiblePlays
---------------------------------------------
findStartsWith :: Player -> Neighbors -> [(String, BoardIdx)]
findStartsWith player ns = mapMaybe (checkStart player) neighborLists
  where
    neighborLists =
      [ ("left", left ns)
      , ("right", right ns)
      , ("up", up ns)
      , ("down", down ns)
      , ("diagNW", diagNW ns)
      , ("diagNE", diagNE ns)
      , ("diagSW", diagSW ns)
      , ("diagSE", diagSE ns)
      ]
    checkStart WhiteP (dir, a: (x, Black) : rest) = Just (dir, a: (x, Black) : rest)
    checkStart BlackP (dir, a: (x, White) : rest) = Just (dir, a: (x, White) : rest)
    checkStart _ _ = Nothing

---------------------------------------------
-- Combine multiple direction possible move keys into one list
-- Uses possiblePlays to get the list of possible move indexes (keys)
-- ***Not efficient because it filters over every unique key
--    maybe use a HashMap?
---------------------------------------------
allDirectionPlays :: Player -> Board -> [BoardIdx]
allDirectionPlays player board = do
    let possible = possiblePlays player board
    let uniqueIdx = nub $ map getFstIdx possible
    [(idx,Empty): concatMap tail (filter ((==idx) . getFstIdx) possible) | idx <- uniqueIdx]

---------------------------------------------
--
--          Move making functions
--
---------------------------------------------

---------------------------------------------
-- flipCells assumes the move is valid
-- Flips all the cells in the list of cells
-- use with allDirectionPlays
--
-- TODO: Rewrite to flip all pieces at once rather than one-by-one
--
---------------------------------------------
flipCells :: Player -> Board -> BoardIdx -> Board
flipCells player = foldr (setCell player . fst)

---------------------------------------------
-- setCell is a helper function to set a cell to a player
-- Used in flipCells
-- possibly inefficient due to list concatenation
---------------------------------------------
setCell :: Player -> Int -> Board -> Board
setCell player cell board = before ++ isP player : after
    where (before, _ : after) = splitAt cell board

---------------------------------------------
--          
--          Position Evaluation functions
--
---------------------------------------------

---------------------------------------------
-- getScore is O(n) because it makes a single pass through the board
---------------------------------------------
getScore :: Board -> Score
getScore = foldr acc (Score 0 0) 
    where 
      acc Black (Score b w) = Score (b+1) w
      acc White (Score b w) = Score b (w+1)
      acc _ s = s

---------------------------------------------
-- weights from https://github.com/norvig/paip-lisp/blob/main/docs/chapter18.md
---------------------------------------------
weights :: [Int]
weights = [
          120, -20, 20,  5,  5, 20, -20, 120, 
          -20, -40, -5, -5, -5, -5, -40, -20, 
           20,  -5, 15,  3,  3, 15,  -5,  20, 
            5,  -5,  3,  3,  3,  3,  -5,   5, 
            5,  -5,  3,  3,  3,  3,  -5,   5, 
           20,  -5, 15,  3,  3, 15,  -5,  20, 
          -20, -40, -5, -5, -5, -5, -40, -20, 
          120, -20, 20,  5,  5, 20, -20, 120
          ]

---------------------------------------------
-- evalWeights is O(n) because it makes a single pass through the board
---------------------------------------------
evalWeights :: Player -> Board -> Int
evalWeights player board = 
    foldr acc 0 $ zip board weights
    where 
      acc (c, w) s 
        | c == isP player = s + w 
        | c == opP player = s - w
        | otherwise = s

---------------------------------------------
-- maxMove is uses evalWeights to find the best move
-- Returns the best move and the evaluation
-- no valid move returns (0, [])
---------------------------------------------
maxMove :: Player -> Board -> (Int, BoardIdx)
maxMove player board = do
    let validMoves = allDirectionPlays player board
    let scores = map (evalWeights player . flipCells player board) validMoves
    foldr acc (minBound :: Int,[]) (zip scores validMoves)
    where acc (w, ms) (w', ms') = if w > w' then (w, ms) else (w', ms')

-- TODO: Minmax depth search


---------------------------------------------
-- 
--          Game loop
--
---------------------------------------------
gameLoop :: Board -> IO ()
gameLoop board = do
    showBoard board
    -- user inputs a move as black
    let possHmoves = allDirectionPlays BlackP board
    if  (not . null) possHmoves then do
        putStrLn "Your move!"
        hMove <- getInput possHmoves
        print hMove -- debugging
        let hFlips = concat $ filter ((==hMove) . getFstIdx) possHmoves
        print hFlips -- debugging
        let board' = flipCells BlackP board hFlips
        showBoard board'
    -- computer responds as white
        let (eval, cMove) = maxMove WhiteP board'
        if (not . null) cMove then do
            print $ "I choose: " ++ idxToAlg (getFstIdx cMove) ++ "!"
            print $ "Eval: " ++ show eval -- debugging
            let board'' = flipCells WhiteP board' cMove
            gameLoop board''
        else do
            putStrLn "No valid move for me!"
            gameLoop board'
    else do
        let score = getScore board
        putStrLn $ "Game Over! Final Score: Black" 
          ++ show (blackS score) 
          ++ " White: " 
          ++ show (whiteS score) 

---------------------------------------------
--
--          I/O functions
--
---------------------------------------------


---------------------------------------------
-- getInput is given a list of legal moves as a parameter
---------------------------------------------
getInput :: [BoardIdx] -> IO Int
getInput legalMoves = do
    putStrLn "Enter a move (e.g., a1):"
    inputLine <- getLine
    case parseInput inputLine legalMoves of
        Just i -> return i
        Nothing -> putStrLn "Invalid move" >> getInput legalMoves

---------------------------------------------
-- parseInput looks for the move to be in the correct form and
-- legal based on the parameter passed of legal moves
---------------------------------------------
parseInput :: String -> [BoardIdx] -> Maybe Int
parseInput s legalMoves =
  if length s == 2 &&
    all (`elem` ['a'..'h']) (take 1 s) &&
    all (`elem` ['1'..'8']) (drop 1 s) &&
    elem (algToIdx s) (map fst (concat legalMoves)) -- move is possible
    then Just $ algToIdx s
    else Nothing

---------------------------------------------
--  showBoard shows the board. obviously.
---------------------------------------------
showBoard :: Board -> IO ()
showBoard board = do
    putStrLn ""
    putStrLn "  a b c d e f g h"
    mapM_ putStrLn (zipWith showRow [1..] (chunksOf boardN board))
    putStrLn "  a b c d e f g h"
    let score = getScore board
    putStrLn $ "  Black: " ++ show (blackS score)  ++ " White: " ++ show (whiteS score) 
    putStrLn ""
  where
    showRow rowIndex xs = show rowIndex ++ " " ++ unwords (map show xs) ++ " " ++ show rowIndex -- Row number


---------------------------------------------
--
--          Main function
--
---------------------------------------------

main :: IO ()
main = do
    putStrLn "Let's Play Othello!"
    putStrLn "You get the black pieces and I get the white ones."
    putStrLn "You go first!"
    gameLoop initialBoard
    -- mapM_ printMove (testMoves 5 initialBoard) -- test computer plays itself
    putStrLn "TO DO: min-max depth search, alpha-beta pruning"



---------------------------------------------
--
--        Debugging stuff
--
---------------------------------------------

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

printMove :: (Board, Player, Maybe Int) -> IO ()
printMove (board, player, idx) = do
    putStrLn $ "Player: " ++ show (opP player)
    case idx of
        Just i -> putStrLn $ "Moved: " ++ show (idxToAlg i)
        Nothing -> putStrLn "No move"
    showBoard board

---------------------------------------------
--
--          Test cases
--
---------------------------------------------

-- some tests
tb1 :: Board
tb1 = setCell WhiteP 20 initialBoard
tb2 :: Board
tb2 = setCell WhiteP 12 tb1

tb1_poss :: [BoardIdx]
tb1_poss = allDirectionPlays BlackP tb2

tmove1 :: Board
tmove1 = flipCells BlackP initialBoard (head $ allDirectionPlays BlackP initialBoard)

tmove2 :: Board
tmove2 = flipCells WhiteP tmove1 (head $ allDirectionPlays WhiteP tmove1)

tmove3 :: Board
tmove3 = flipCells BlackP tmove2 (head $ allDirectionPlays BlackP tmove2)

testMoves :: Int -> Board -> [(Board, Player, Maybe Int)]
testMoves n board = take (n + 1) $ iterate nextMove (board, BlackP, Nothing)
  where
    nextMove (b, player, _) =
      case allDirectionPlays player b of
        (move : _) -> (flipCells player b move, switchP player, Just (fst $ head move))
        []         -> (b, switchP player, Nothing)  -- Skip if no valid move

testWeights :: Int -> Board -> [(Board, Player, Maybe Int)]
testWeights n board = take (n + 1) $ iterate nextMove (board, BlackP, Nothing)
  where
    nextMove (b, player, _) = 
      let (eval, best) = maxMove player b
          b' = flipCells player b best in
      case best of
        [] -> (b', switchP player, Nothing)
        _  -> (b', switchP player, Just $ getFstIdx best)

-- a board with flips in multiple directions, for example black c1
edgeBoard :: Board
edgeBoard = do
  let b = foldr (setCell BlackP) initialBoard blackCells
  let w = foldr (setCell WhiteP) b whiteCells
  w

  where blackCells = [16,17,18,19,20,21,22,23]
        whiteCells = [8,9,10,11,12,13,14,15]


---------------------------------------------
--
--          Dead Code
--
---------------------------------------------

-- Not using walls for out of bounds

-- data Cell = Empty | Black | White
--  | Wall
--   deriving (Eq)

-- data Player = BlackP | WhiteP
--   deriving (Eq, Show)

-- instance Show Cell where
--     show :: Cell -> String
--     show Empty = "·"
--     show Black = "○"
--     show White = "●"
--     show Wall  = "█"

-- boardN :: Int
-- boardN = 8 -- change to 10 if we use walls for out of bound

-- With walls for out of bounds. Not sure we need this.
-- initialBoard :: Board
-- initialBoard =
--     replicate 10 Wall ++
--     take 30 (cycle (Wall : replicate 8 Empty ++ [Wall])) ++
--     (Wall : replicate 3 Empty ++ [White, Black] ++ replicate 3 Empty ++ [Wall]) ++
--     (Wall : replicate 3 Empty ++ [Black, White] ++ replicate 3 Empty ++ [Wall]) ++
--     take 30 (cycle (Wall : replicate 8 Empty ++ [Wall])) ++
--     replicate 10 Wall

-- showBoard :: Board -> IO ()
-- showBoard board = do
--     putStrLn ""
--     putStrLn "  a b c d e f g h"
--     mapM_ putStrLn (zipWith showRow [1..] (chunksOf boardN board))
--     putStrLn "  a b c d e f g h"
--     let blackCount = length (filter (== Black) board)
--     let whiteCount = length (filter (== White) board)
--     putStrLn $ "  Black: " ++ show blackCount ++ " White: " ++ show whiteCount
--     putStrLn ""
--   where
--     showRow rowIndex xs = show rowIndex ++ " " ++ unwords (map show xs) ++ " " ++ show rowIndex -- Row number
--         | all (== Wall) xs = "  "
--         | otherwise = show rowIndex ++ " " ++ unwords (map show (init (tail xs))) ++ " " ++ show rowIndex -- Row number


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