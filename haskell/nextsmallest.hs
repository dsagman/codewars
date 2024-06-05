{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
-- Write a function that takes a positive integer and
-- returns the next smaller positive integer containing
-- the same digits.
-- For example:
-- nextSmaller(21) == 12
-- nextSmaller(531) == 513
-- nextSmaller(2071) == 2017
-- Return -1 (for Haskell: return Nothing, for Rust: return None),
-- when there is no smaller number that contains the same digits.
-- Also return -1 when the next smaller number with the
-- same digits would require the leading digit to be zero.
-- nextSmaller(9) == Nothing
-- nextSmaller(135) == Nothing
-- nextSmaller(1027) == Nothing -- 0721 is out since we don't
-- write numbers with leading zeros
-- some tests will include very large numbers.
-- test data only employs positive integers.
-- The function you write for this challenge is the
-- inverse of this kata: "Next bigger number with the same digits."
import Test.Hspec ( describe, it, shouldBe, Spec, hspec)
import Data.List ( delete, sort, find, (\\))


nextSmaller :: Integer -> Maybe Integer
nextSmaller n
  | null s = Nothing
  | head s == 0 = Nothing
  | otherwise = Just $ makeListI s
  where s = concatMap reverse $ reverse $ swapPivotAndSort n

splitFirstLT :: Ord a => [a] -> [[a]]
splitFirstLT [x] = [[x],[]]
splitFirstLT (x:y:xs)
  | x < y = [x] : [y : xs]
  | otherwise = (x:head s1) : tail s1
  where s1 = splitFirstLT (y:xs)

swapPivotAndSort :: Integer -> [[Integer]]
swapPivotAndSort n
  | null t = []
  | otherwise = sort (beforePivot ++ [pivot2]) :  [pivot1 : afterPivot]
  where s = splitFirstLT $ reverse $ makeIList n
        h = head s
        t = concat $ tail s
        beforePivot = delete pivot1 h
        pivot1 = maximum (filter (<pivot2) h)
        pivot2 = head t
        afterPivot = tail t

makeIList :: Integer -> [Integer]
makeIList = map ((read :: String -> Integer) . (:"")) . show

makeListI :: [Integer] -> Integer
makeListI = (read :: String -> Integer) . concatMap show

-- someone else came up with the below
-- really slow for worst case 90123456789
next :: (Show a, Num a, Enum a) => a -> Maybe a
next n = find ((== sort (show n)) . sort . show) [n - 1, n - 2 .. 0]

next' :: Integer -> Maybe Integer
next' n = find (\x -> null (show n \\ show x)) [n - 1, n - 2 .. 0]


spec :: Spec
spec = describe "Next smaller number with same digits" $ do
    it "Small basic tests" $ do
      nextSmaller 907 `shouldBe` Just 790
      nextSmaller 531 `shouldBe` Just 513
      nextSmaller 135 `shouldBe` Nothing
      nextSmaller 1027 `shouldBe` Nothing
      nextSmaller 414 `shouldBe` Just 144

    it "Larger basic test" $ do
      nextSmaller 123456798 `shouldBe` Just 123456789
      nextSmaller 123456789 `shouldBe` Nothing
      nextSmaller 1234567908 `shouldBe` Just 1234567890


-- fix leading zeros : delete m xs))


-- start on right side
-- first pair with increase in left side, left side is pivot
-- swapper' with pivot
-- sort descending to right of pivot (included in swapper')

-- maxBelowX :: Ord a => [a] -> a
-- maxBelowX (x:xs) = maximum (filter (<x) xs)

-- 531 -> 513 : [], [5,3,1] works
-- 513 -> 351 : [], [5,1,3] wrong: 153
-- 351 -> 315 : [3], [5,1] fst ++ reverse snd
-- 315 -> 153 : []. [3,1,5] wrong: 135
-- 153 -> 135 : [1], [5,3] fst ++ reverse snd
-- 135 -> 135 : [1,3,5], [] null snd -> nothing

-- test = mapM_ print $ zip4 a (map sF a) (map swapPivotAndSort a) (map nextSmaller a)
-- a = [531,513,351,315,153,135]

-- sF = splitFirstLT . reverse . makeIList


-- nextSmaller :: Integer -> Maybe Integer
-- nextSmaller n = Just ans
--   -- | ans >= n = Nothing
--   -- | otherwise =  Just ans
--   where
--     ans = makeListI $ reverse (fixEnd (reverse $ makeIlist n))

-- nextSmaller :: Integer -> Maybe Integer
-- nextSmaller n
--   | null s = Just $ makeListI f
--   | otherwise = Just $ makeListI $ f ++  (fixEnd ( s))
--   where (f,s) = splitSmall (makeIlist n)

-- nextSmaller :: Integer -> Maybe Integer
-- nextSmaller n
--   | null s = Nothing
--   | otherwise = Just $ makeListI $ f ++ reverse (fixEnd (reverse s))
--   where (f,s) = splitSmall (makeIlist n)

-- -- fixEnd :: [Integer] -> [Integer]
-- fixEnd [x,y]
--     | x > y = [y,x]
--     | otherwise = [x,y]
-- fixEnd (x:y:xs)
--    | y == 0 = noSwap
--    | x > y = noSwap
--    | otherwise = swap
--    where
--      noSwap = x : fixEnd (y:xs)
--      swap   = y : fixEnd (x:xs)

-- splitSmall :: [Integer] -> ([Integer], [Integer])
-- splitSmall [x,y]
--     | x < y = ([x,y],[])
--     | otherwise = ([],[x,y])
-- splitSmall (x:y:xs)
--    | x < y = (x : fst sSm, snd sSm)
--    | otherwise = ([], x:y:xs)
--    where sSm = splitSmall (y:xs)

-- digitScan [x,y]
--    | x < y = [y,x]
--    | otherwise = [x,y]
-- digitScan (x:y:xs)
--    | x < y =  m : y :  x
--    | otherwise = x : digitScan (y:xs)
--    where m = maximum (filter (<y) (x:xs))

-- dS  =  reverse . digitScan . reverse . makeIList
-- swapper :: Ord a => [a] -> [[a]]
-- swapper (x:xs) = [m : reverse (sort (x : delete m xs)), x :  (sort xs)]
--   where m = maximum (filter (<x) xs)


-- swapper' :: Ord a => [a] -> [a]
-- swapper' (x:xs) = m : reverse (sort (x : delete m xs))
--   where m = maximum (filter (<x) xs)
-- newEnd :: [Integer] -> [Integer]
-- newEnd xs
--    | j < n = makeIList j
--    | i < n = makeIList i
--   --  | otherwise = Nothing
--    where [i, j] = sort $ makeListI <$> swapper xs
--          n = makeListI xs


-- Does not work for time complexity. Permutations get out of control.
-- nextSmaller :: Integer -> Maybe Integer
-- nextSmaller n
--       | null possible = Nothing
--       | otherwise = Just $ last possible
--       where possible = filter (<n) (orderings n)


-- orderings :: Integer -> [Integer]
-- orderings n = sort $ makeI $ filter (\x -> length x == length showN) $
--               makeS $ makeI $ permutations showN
--               where
--                 showN = show n
--                 makeI = map (read :: String -> Integer)
--                 makeS = map (show :: Integer -> String)
