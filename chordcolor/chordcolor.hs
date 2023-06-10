module Main where

import Data.List ( intercalate )

main :: IO ()
main = do
    print "I will make a chord for you." 
    print "Root note?"
    root <- getLine
    print "Color? (M, m, dim, or aug)"
    color <- getLine
    putStr "Your chord is: " 
    print $ intercalate " " $ chordTriad root color

notes :: [String]
notes =  cycle ["A", "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#"]

chordTriad :: [Char] -> [Char] -> [[Char]]
chordTriad root color = zipWith3 (\x y z -> formatAccent(x ++ y ++ z))
                        (major rawRoot) modify cycleOrigAccent
        where   rawRoot = [head root]
                cycleOrigAccent = cycle [tail root]
                modify = case color of
                    "M"   ->  ["", "" , "" ]
                    "m"   ->  ["", "b", "" ] 
                    "dim" ->  ["", "b", "b"] 
                    "aug" ->  ["", "" , "#"] 

formatAccent :: [Char] -> [Char]
formatAccent note =
    case origAccent of
        "#b" -> rawRoot
        "b#" -> rawRoot
        "##" -> rawRoot ++ "x"
        _ -> note
    where rawRoot = [head note]
          origAccent = tail note

major :: String -> [String]
major root = tone 0 ++ tone 4 ++ tone 7
    where tone n = [head(drop n mynotes)] 
          mynotes = dropWhile (/=root) notes
