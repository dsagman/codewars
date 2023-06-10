import Data.List (partition)




t1 :: [Integer]
t1 = [2, 4, 0, 100, 4, 11, 2602, 36]
t2 :: [Integer]
t2 = [2, 4, 6, 8, 10, 3]
t3 :: [Integer]
t3 = [160, 3, 1719, 19, 11, 13, -21]

tlist = [   [2, 4, 0, 100, 4, 11, 2602, 36],
            [2, 4, 6, 8, 10, 3],
            [160, 3, 1719, 19, 11, 13, -21]]

eval :: [Integer] -> Integer
eval xs  
    | ln == 1 = (head . dropWhile odd) xs
    | otherwise = (head . dropWhile even) xs
    where ln = (length . filter even) xs


eval' :: [Integer] -> Integer
eval' xs 
    | length (fst ln) == 1 = head $ fst ln
    | otherwise = head $ snd ln
    where ln = partition even xs