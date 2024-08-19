module Main where

import Data.List
import Data.Char
import Data.Maybe

wordCheck :: String -> String -> Bool
wordCheck ps xs = not $ any id $ zipWith (==) idxes (tail idxes)
    where idxes = map (flip div 3 . fromMaybe (-1 ). (`elemIndex` ps)) xs
          

main :: IO()
main = do
  let puzzle = "enrtblaoipxs"
  words <- readFile "/usr/share/dict/words"
  -- valid words are 3 to 9 characters, a vowel, no repeating letters,
  -- no uppercase, and all letters are in the puzzle
  let wordsBase = filter (\w -> length w >= 3 && length w <= 9 && 
                    any (`elem` "aeiou") w && 
                    length w == length (nub w) && 
                    all isLower w &&
                    all (`elem` puzzle) w) $ lines words
  let wordsValid = filter (wordCheck puzzle) wordsBase
  let twoWordCombos = [(w1, w2) | w1 <- wordsValid, w2 <- wordsValid, last w1 == head w2]  
  let twoWordSolutions = [(w1, w2) | (w1, w2) <- twoWordCombos, length (nub (w1 ++ w2)) == length puzzle]
  
  let threeWordCombos = [(w1, w2, w3) | (w1, w2) <- twoWordCombos, w3 <- wordsValid, last w2 == head w3]
  let threeWordSolutions = [(w1, w2, w3) | (w1, w2, w3) <- threeWordCombos, length (nub (w1 ++ w2 ++ w3)) == length puzzle]
  
  -- four words takes a long time
--   let fourWordCombos = [(w1, w2, w3, w4) | (w1, w2, w3) <- threeWordCombos, w4 <- wordsValid, last w3 == head w4]
--   let fourWordSolutions = [(w1, w2, w3, w4) | (w1, w2, w3, w4) <- fourWordCombos, length (nub (w1 ++ w2 ++ w3 ++ w4)) == length puzzle]

  putStrLn $ "---------------------------"
  putStrLn $ "Words loaded: " ++ show(length $ lines words)
  putStrLn $ "Puzzle:" ++ show puzzle
  putStrLn $ "Valid words: " ++ show(length wordsValid)
  putStrLn $ "Two Word Combos: " ++ show(length twoWordCombos)
  putStrLn $ "Two Word Solutions:" ++ show(length twoWordSolutions)
  mapM_ print $ twoWordSolutions 
  putStrLn $ "Three Word Combos: " ++ show(length threeWordCombos)
  putStrLn $ "Three Word Solutions: " ++ show(length threeWordSolutions)
--   putStrLn $ "Four Word Combos: " ++ show(length fourWordCombos)
--   putStrLn $ "Four Word Solutions: " ++ show(length fourWordSolutions)
  putStrLn $ "---------------------------"
  
