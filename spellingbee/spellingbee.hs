main :: IO()
main = do
  let ctr = 'i'
  let six = "todrym"
  let dictFile = "words_alpha.txt"
  wordList <- readFile dictFile
  let solution = filter (\w -> length w > 3
                        && elem ctr w
                        && all (\c -> c `elem` (ctr:six)) w)
                $ lines wordList
  let pangram = filter (\w -> countSix six w == 6) solution
  print solution
  print $ length solution
  print pangram

countSix :: (Foldable t, Eq a) => [a] -> t a -> Int
countSix six word = length [s | s <- six, s `elem` word]

-- all to avoid importing Data.List.nub

