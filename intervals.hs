-- Intervals are represented by a pair of integers in the form of an array. The first value of the interval will always be less than the second value. Interval example: [1, 5] is an interval from 1 to 5. The length of this interval is 4.

-- Overlapping Intervals
-- List containing overlapping intervals:

-- [
--    [1,4],
--    [7, 10],
--    [3, 5]
-- ]
-- The sum of the lengths of these intervals is 7. Since [1, 4] and [3, 5] overlap, we can treat the interval as [1, 5], which has a length of 4.

-- Examples:
-- sumOfIntervals([(1, 5}, (10, 15}, (-1, 3)]) -- => 11

-- sumOfIntervals([(1, 5)]) -- => 4
import Test.Hspec ( it, shouldBe, Spec, hspec )
import Control.Monad ( ap, liftM2, join )
import Data.Bifunctor (bimap)
import Data.List (sort, partition)

-- someone else found this
sumOfIntervals' :: (Num a, Ord a) => [(a, a)] -> a
sumOfIntervals' = sum . map (uncurry subtract) . scanl1 (join bimap . max . snd) . sort

sumOfIntervals :: (Num a, Ord a) => [(a, a)] -> a
sumOfIntervals = sum . map (liftM2 (-) snd fst) . removeAllOverlaps
-- sumOfIntervals i = sum $ map (\x -> snd x - fst x) $ removeOverlapsOnce i

removeAllOverlaps :: (Num a, Ord a) => [(a, a)] -> [(a, a)]
removeAllOverlaps intervals
   | length intervals == length remOnce = intervals
   | otherwise = removeAllOverlaps remOnce
   where remOnce = removeOverlapsOnce intervals

-- rr :: Ord a => [(a, a)] -> ([(a, a)], [(a, a)])
-- rr = partition =<< olap . head


-- s :: Ord a => [(a,a)] -> (a, a)
-- s = foldl1 (\(x1, y1) (x2, y2) -> ((min x1 x2), (max y1 y2)))

removeOverlapsOnce :: (Num a, Ord a) => [(a, a)] -> [(a, a)]
removeOverlapsOnce [] = []
removeOverlapsOnce [x] = [x]
removeOverlapsOnce intervals = head combine : removeOverlapsOnce (tail combine)
   where
         combine = combineOverlaps intervals

combineOverlaps' :: (Num a, Ord a) => [(a, a)] -> [(a, a)]
combineOverlaps' intervals = concat $ bimap s id (p intervals)
   where s = foldl1 (\(x1, y1) (x2, y2) -> (min x1 x2, max y1 y2))
         p = partition =<< olap . head

combineOverlaps :: (Num a, Ord a) => [(a, a)] -> [(a, a)]
combineOverlaps intervals = (n,m) : notOverlaps
   where
         hasOverlaps = map fst $ filter snd $ tagOverlaps intervals
         notOverlaps = map fst $ filter (not . snd) $ tagOverlaps intervals
         n = minimum $ map fst hasOverlaps
         m = maximum $ map snd hasOverlaps

tagOverlaps :: Ord a => [(a, a)] -> [((a, a), Bool)]
tagOverlaps = ap zip (map =<< olap . head)
-- tagOverlaps intervals = zip intervals (map (olap (head intervals)) intervals)

olap :: (Ord a) => (a, a) -> (a, a) -> Bool
olap (a, b) (c, d) = (a <= c) && (c <= b) || (a <= d) && (d <= b) || (c <= a) && (a <= d) || (c <= b) && (b <= d)

t1 = [(-137,158),(481,485),(377,462),(249,407),(435,491),(250,365),(-459,-330),(245,434),(-304,497),(-294,293)]

t2 = [(266,458),(407,410),(-84,256),(-76,423),(-119,-99),(262,442),(450,485),(-24,318),(-318,-170),(78,218),(177,500)]


spec :: Spec
spec = it "Basic tests" $ do
  sumOfIntervals [(1, 5)] `shouldBe` 4
  sumOfIntervals [(1, 5), (6, 10)] `shouldBe` 8
  sumOfIntervals [(1, 5), (1, 5)] `shouldBe` 4
  sumOfIntervals [(1, 4), (7, 10), (3, 5)] `shouldBe` 7
  sumOfIntervals [(1, 4), (3, 5)] `shouldBe` 4
  sumOfIntervals [(3, 5), (1, 4)] `shouldBe` 4
  sumOfIntervals [(1, 5), (1, 4)] `shouldBe` 4
  sumOfIntervals [(1, 4), (1, 5)] `shouldBe` 4
  sumOfIntervals [(-137,158),(481,485),(377,462),(249,407),(435,491),(250,365),(-459,-330),(245,434),(-304,497),(-294,293)] `shouldBe` 930
  sumOfIntervals [(266,458),(407,410),(-84,256),(-76,423),(-119,-99),(262,442),(450,485),(-24,318),(-318,-170),(78,218),(177,500)] `shouldBe` 752
