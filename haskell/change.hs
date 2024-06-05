
import Test.Hspec

-- countChange :: Integer -> [Integer] -> Integer

import Data.List.Extra
import Data.Maybe
import Data.Char
import Data.List.Split


-- countChange total coins = do
--       let sortedCoins = reverse $ sort coins
--       (c,cs) <- uncons sortedCoins
--       aChange <- getChange total sortedCoins
--       -- let nChange = map (reverse . sort) (aChange : nextChange aChange cs)
--       let nChange = nubOrd $ map sort (aChange : nextChange aChange cs)
--       -- let nChange = nubOrd $ map sort (aChange : filter (\x -> sum x == total) (nextChange aChange cs))
--       let count   = length nChange
--       return nChange



getChange :: Integer -> [Integer] -> Maybe [Integer]
getChange total coins = do
      -- let sortedCoins = reverse $ sort coins
      -- isn't the list already sorted?
      (c,cs) <- uncons coins
      let change = replicate (fromInteger $ div total c) c
      let extraChange = getChange (total-sum change) cs
      return $ change ++ fromMaybe [] extraChange

-- nextChange :: [Integer] -> [Integer] -> [[Integer]]
-- nextChange totals coins
--       | null totals      = []
--       | null coins       = []
--       | length totals ==1 = mE
--       | otherwise        = mE ++
--                            concatMap (\x -> nextChange x (tail coins)) mE
--             where mE = mapEach totals coins

-- mapEach :: [Integer] -> [Integer] -> [[Integer]]
-- -- rewrite mapEach to apply the map to every subset and leave intact
-- -- the rest of set minus the subset
-- mapEach xs ys = zipWith (++) ((map . concatMap) fn (inits xs)) (tails xs)
--       where fn x = fromMaybe [] $ getChange x ys
mapEach xs ys = zip xs (map fn xs)
      where fn x = fromMaybe [] $ getChange x ys

-- tupleListToList :: [([a], [a])] -> [[a]]
-- tupleListToList = map (uncurry (++))
splitUp xs = inits $ zipWith (:) (inits xs)
                    (zipWith (\a b-> [a,b]) (map (take 1) (tails xs))
                                            (map (drop 1) (tails xs)))

b :: [(Integer, Integer)]
b = zip (replicate 10 1) (replicate 10 0)

eachOne xs = [[if i /= j then fst (xs!!i) else snd (xs!!j) | i <- n] | j <- n]
            where n = [0..length xs-1]







t0 :: [Integer]
t0 = [1,2]
t1 ::[Integer]
t1 = [1]
t2 :: [Integer]
t2 = [500,5,50,100,20,200,10]
t3 :: [Integer]
t3 = [3,2,4,1]



-- spec :: Spec
-- spec = do
--   describe "Basic Tests" $ do
--     it "countChange 2 [1] => 1" $
--       countChange 2 [1] `shouldBe` 1
--     it "countChange 2 [1,2] => 2" $
--       countChange 2 [1,2] `shouldBe` 2
--     it "countChange 3 [1,2] => 2" $
--       countChange 3 [1,2] `shouldBe` 2
--     it "countChange 4 [1,2] => 3" $
--       countChange 4 [1,2] `shouldBe` 3
--     it "countChange 4 [2] => 1" $
--       countChange 4 [2] `shouldBe` 1
--     it "countChange 10 [3,2,4,1] => 23" $
--       countChange 10 [3,2,4,1] `shouldBe` 23
--     it "countChange 300 [500,5,50,100,20,200,10] => 1022" $
--       countChange 300 [500,5,50,100,20,200,10] `shouldBe` 1022