module Main where

import Data.List ( elemIndex, intercalate, isInfixOf)
import Data.List.Extra ( chunksOf )
import Data.Char ( isLower, toLower, isLetter )
import Data.Maybe ( fromMaybe )
import Control.Monad ( when )
import System.IO (hFlush, stdout)
import System.Console.ANSI (setSGRCode, SGR(..), Color(..), ColorIntensity(..), ConsoleLayer(..))
import qualified Data.Map as M
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Data.Set as S
import Text.Printf (printf)


-- time to beat for 3 words is 3 seconds
getPuzzleInput :: IO String
getPuzzleInput = do
  -- let defaultPuzzle = "xnimalpjyegf"
  let defaultPuzzle = "nosumailtcvr"
  putStrLn "Enter the puzzle, no blanks, start at top left:"
  putStrLn "Example [default]: xnimalpjyegf"
  puzzle <- getLine
  if null puzzle
    then return defaultPuzzle -- Use the default puzzle if input is empty
    else if length puzzle /= 12 || not (all isLetter puzzle)
      then do
        putStrLn "Puzzle must be 12 characters long and contain only letters."
        getPuzzleInput -- Recurse until the input is valid
      else return puzzle

wordCheck :: String -> String -> Bool
wordCheck ps xs = not $ or $ zipWith (==) idxes (tail idxes)
    where idxes = map (flip div 3 . fromMaybe (-1) . (`elemIndex` ps)) xs

nWordCombo :: [String] -> M.Map Char [String] -> Bool -> [String]
nWordCombo existCombos firstCharMap reqUniq =
          [ combo ++ " " ++ w
          | combo <- existCombos         -- memoize the existing combos
          , Just ws <- [M.lookup (last combo) firstCharMap]
          , w <- ws
          , not $ isInfixOf w combo
          , not reqUniq || S.size (S.fromList (combo ++ w)) == length (combo ++ w)  -- Check uniqueness if required 
          ]

nWordSol :: [String] -> Int -> [String]
nWordSol combos lenP = [x |
                          x <- combos
                          , S.size (S.fromList x) == lenP + 1
                          ]

main :: IO()
main = do
  puzzle <- getPuzzleInput
  let lenP = length puzzle
  putStrLn $ "You entered: " ++ puzzle
  putStr "Do you want no duplicate letters in 3, 4 and 5 word solutions? (y/[n])"
  hFlush stdout
  reqUniqIn <- getChar
  let reqUniq = case toLower reqUniqIn of
                  'y' -> True
                  'n' -> False
                  _   -> False

  -- let dictFile = "/usr/share/dict/words"
  let dictFile = "words_alpha.txt"
  wordList <- readFile dictFile
  let wordsBase = filter (\w -> length w >= 3 && length w <= 9 &&
                    all isLower w &&
                    all (`elem` puzzle) w) $ lines wordList
  let wordsValid = filter (wordCheck puzzle) wordsBase

  let firstCharMap = M.fromListWith (++) [(head w, [w]) | w <- wordsValid]

  let twoWordCombos = nWordCombo wordsValid firstCharMap False
  let twoWordSolutions = nWordSol twoWordCombos lenP

  let threeWordCombos = nWordCombo twoWordCombos firstCharMap reqUniq
  let threeWordSolutions = nWordSol threeWordCombos lenP

  let fourWordCombos = nWordCombo threeWordCombos firstCharMap reqUniq
  let fourWordSolutions = nWordSol fourWordCombos lenP

  let fiveWordCombos = nWordCombo fourWordCombos firstCharMap reqUniq
  let fiveWordSolutions = nWordSol fiveWordCombos lenP

  putStrLn ""
  putStrLn "---------------------------"
  putStrLn $ "Words loaded: " ++ addCommas (length $ lines wordList)
  putStrLn $ "Puzzle:" ++ show puzzle
  putStrLn $ "Valid words: " ++ addCommas (length wordsValid)
  goOutput "Two" twoWordCombos twoWordSolutions
  goOutput "Three" threeWordCombos threeWordSolutions
  goOutput "Four" fourWordCombos fourWordSolutions
  goOutput "Five" fiveWordCombos fiveWordSolutions
  when reqUniq $ putStrLn "*Three, four and five word solutions have no repeating letters"
  putStrLn "---------------------------"

goOutput :: String -> [[Char]] -> [[Char]] -> IO ()
goOutput n combos sols = do
  let solN = 10 -- number of solutions to display
  sTime <- getCurrentTime
  putStrLn $ n ++ " Word Combos: " ++ addCommas (length combos)
  putStrLn $ n ++ " Word Solutions: " ++ addCommas (length sols) ++ ", sample below"
  eTime <- getCurrentTime
  printf "%s Word Time Elapsed: %.2f seconds\n" n (realToFrac (diffUTCTime eTime sTime) :: Double)
  mapM_ textFormat $ firstOfSublist solN sols


addCommas :: Int -> String
addCommas n = reverse $ intercalate "," $ chunksOf 3 $ reverse $ show n

textFormat :: [Char] -> IO ()
textFormat = putStrLn . (("\t" ++) . (blueText ++) . (++ resetText))

blueText :: String
blueText = setSGRCode [SetColor Foreground Vivid Blue] 

resetText :: String
resetText = setSGRCode [Reset] 

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


  -- parallel had too much overhead
  -- let chunksize = 10000
  -- `using` parListChunk chunksize rseq
  -- `using` parList rseq