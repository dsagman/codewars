module Main where

import Data.List ( elemIndex, intercalate )
import Data.List.Extra ( nubOrd, chunksOf )
import Data.Char ( isLower, toLower, isLetter )
import Data.Maybe ( fromMaybe )
import Control.Monad ( when )

wordCheck :: String -> String -> Bool
wordCheck ps xs = not $ or $ zipWith (==) idxes (tail idxes)
    where idxes = map (flip div 3 . fromMaybe (-1 ). (`elemIndex` ps)) xs

addCommas :: Int -> String
addCommas n = reverse $ intercalate "," $ chunksOf 3 $ reverse $ show n

firstOfSublist :: Int -> [a] -> [a]
firstOfSublist n xs = map head $ filter (not . null) (splitIntoN n xs)

splitIntoN :: Int -> [a] -> [[a]]
splitIntoN n ys = go sizes ys
  where
    len = length ys
    (q, r) = len `divMod` n
    sizes = replicate r (q + 1) ++ replicate (n - r) q
    go [] _ = []
    go (s:ss) zs = take s zs : go ss (drop s zs)

getPuzzleInput :: IO String
getPuzzleInput = do
  putStrLn "Enter the puzzle, no blanks, start at top left:"
  putStrLn "Example: xnimalpjyegf"
  puzzle <- getLine
  if length puzzle /= 12 || not (all isLetter puzzle)
    then do
      putStrLn "Puzzle must be 13 characters long and contain only letters."
      getPuzzleInput -- Recurse until the input is valid
    else return puzzle

main :: IO()
main = do
  puzzle <- getPuzzleInput
  putStrLn $ "You entered: " ++ puzzle
  putStr "Do you want to require no repeating letters in 3 and 4 word solutions? (y/n)"
  reqUniqIn <- getChar
  let reqUniq = case toLower reqUniqIn of
                  'y' -> True
                  'n' -> False
                  _   -> False  -- Default to False if the input is invalid

  -- let dictFile = "nytimes_games/words_alpha.txt" 
  let dictFile = "/usr/share/dict/words"
  wordList <- readFile dictFile
  -- valid words are 3 to 9 characters, a vowel, no uppercase, and all letters are in the puzzle
  let wordsBase = filter (\w -> length w >= 3 && length w <= 9 &&
                    all isLower w &&
                    all (`elem` puzzle) w) $ lines wordList
  let wordsValid = filter (wordCheck puzzle) wordsBase

  let twoWordCombos = [(w1, w2) | w1 <- wordsValid
                      , w2 <- wordsValid
                      , last w1 == head w2
                      , w1 /= w2]
  let twoWordSolutions = [(w1, w2) | (w1, w2) <- twoWordCombos
                      , length (nubOrd (w1 ++ w2)) == length puzzle]

  -- for three and four word solutions, don't allow repeating letters
  -- greatly reduces search space
  let threeWordCombos = [(w1, w2, w3) | (w1, w2) <- twoWordCombos
                        , w3 <- wordsValid
                        , last w2 == head w3
                        , w1 /= w3, w2 /= w3
                        , let ww = w1 ++ tail w2 ++ tail w3
                        , not reqUniq ||length (nubOrd ww) == length ww ]
  let threeWordSolutions = [(w1, w2, w3) | (w1, w2, w3) <- threeWordCombos
                        , length (nubOrd (w1 ++ w2 ++ w3)) == length puzzle]


  let fourWordCombos = [(w1, w2, w3, w4) | (w1, w2, w3) <- threeWordCombos
                        , w4 <- wordsValid
                        , last w3 == head w4
                        , w1 /= w4, w2 /= w4, w3 /= w4
                        , let ww = w1 ++ tail w2 ++ tail w3 ++ tail w4
                        , not reqUniq || length (nubOrd ww) == length ww]
  let fourWordSolutions = [(w1, w2, w3, w4) | (w1, w2, w3, w4) <- fourWordCombos
                        , length (nubOrd (w1 ++ w2 ++ w3 ++ w4)) == length puzzle]

  let solN = 10 -- number of solutions to display
  putStrLn ""
  putStrLn "---------------------------"
  putStrLn $ "Words loaded: " ++ addCommas (length $ lines wordList)
  putStrLn $ "Puzzle:" ++ show puzzle
  putStrLn $ "Valid words: " ++ addCommas (length wordsValid)
  putStrLn $ "Two Word Combos: " ++ addCommas (length twoWordCombos)
  putStrLn $ "Two Word Solutions:" ++ addCommas (length twoWordSolutions)
  mapM_ (putStrLn . ("\t" ++) . show) twoWordSolutions
  putStrLn $ "Three Word Combos: " ++ addCommas (length threeWordCombos) ++ "sample below"
  putStrLn $ "Three Word Solutions: " ++ addCommas (length threeWordSolutions)
  mapM_ (putStrLn . ("\t" ++) . show) $ firstOfSublist solN threeWordSolutions
  putStrLn $ "Four Word Combos: " ++ addCommas (length fourWordCombos)
  putStrLn $ "Four Word Solutions: " ++ addCommas (length fourWordSolutions)  ++ "sample below"
  mapM_ (putStrLn . ("\t" ++) . show) $ firstOfSublist solN fourWordSolutions
  when reqUniq $ putStrLn "*Three and four word solutions have no repeating letters"
  putStrLn "---------------------------"


-- the below are amazing map implementations and too much overhead. 
-- ran slower than naive implementation above

-- import Data.Map hiding (map, filter)
-- import qualified Data.Map as M 
-- import Control.Monad.RWS (MonadState(put))
-- import Control.Parallel.Strategies

-- headMap :: Ord k => [[k]] -> Map k [[k]]
-- headMap xs = fromListWith (++) [(head x, [x]) | x <- xs]

-- tailMap :: Ord k => [[k]] -> Map k [[k]]
-- tailMap xs = fromListWith (++) [(last x, [x]) | x <- xs]

-- twoWordMap :: String -> [String] -> Map Char [(String, String)]
-- twoWordMap puzzle xs = fromListWith (++) [(k, [(t, h)]) | k <- puzzle, Just hs <- [M.lookup k hm], Just ts <- [M.lookup k tm], h <- hs, t <- ts]
--   where
--     hm = headMap xs  
--     tm = tailMap xs

-- threeWordMap :: (Ord k1, Ord k2) => [k1] -> Map k1 [(a, [k2])] -> [[k2]] -> Map k1 [(a, [k2], [k2])]
-- threeWordMap puzzle twoMap xs = fromListWith (++) 
--     [ (k, [(w1, w2, w3)]) 
--     | k <- puzzle
--     , Just pairs <- [M.lookup k twoMap]
--     , (w1, w2) <- pairs
--     , let tm = last w2
--     , Just ws3 <- [M.lookup tm hm]
--     , w3 <- ws3
--     ]
--   where
--     hm = headMap xs

-- fourWordMap :: (Ord k1, Ord k2) => [k1] -> Map k1 [(a, [k2], [k2])] -> [[k2]] -> Map k1 [(a, [k2], [k2], [k2])]
-- fourWordMap puzzle threeMap xs = fromListWith (++) 
--     [ (k, [(w1, w2, w3, w4)]) 
--     | k <- puzzle
--     , Just triples <- [M.lookup k threeMap]
--     , (w1, w2, w3) <- triples
--     , let tm = last w3
--     , Just ws4 <- [M.lookup tm hm]
--     , w4 <- ws4
--     ]
--   where
--     hm = headMap xs


  -- let twoWordCombos = twoWordMap puzzle wordsValid
  -- let twoWordSolutions = [(w1, w2) | (w1, w2) <- concat (M.elems twoWordCombos), length (nub (w1 ++ w2)) == length puzzle]
  -- let threeWordCombos = threeWordMap puzzle twoWordCombos wordsValid
  -- let threeWordSolutions = [(w1, w2, w3) | (w1, w2, w3) <- concat (M.elems threeWordCombos), length (nub (w1 ++ w2 ++ w3)) == length puzzle]
  -- let fourWordCombos = fourWordMap puzzle threeWordCombos wordsValid
  -- let fourWordSolutions = [(w1, w2, w3, w4) | (w1, w2, w3, w4) <- concat (M.elems fourWordCombos), length (nub (w1 ++ w2 ++ w3 ++ w4)) == length puzzle]

  -- putStrLn $ "Two Word Combos: " ++ show(sum $ map length $ M.elems twoWordCombos)
  -- putStrLn $ "Three Word Combos: " ++ show(sum $ map length $ M.elems threeWordCombos)
  -- putStrLn $ "Four Word Combos: " ++ show(sum $ map length $ M.elems fourWordCombos)


  -- parallel had too much overhead
  -- let chunksize = 10000
  -- `using` parListChunk chunksize rseq
  -- `using` parList rseq