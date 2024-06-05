import Test.Hspec
import Data.List.Split (splitPlaces)

josephus :: [a] -> Int -> [a]
josephus xs k
    | null xs = []
    | length xs == 1 = xs
    | otherwise = b ++ josephus (concat c ++ a) k
        where
            newk = min k (((k-1) `mod` length xs) + 1)
            (a:b:c) = splitPlaces [newk-1, 1, length xs] xs


spec = hspec $  do
  describe "josephus" $ do
    it "works with integers" $ do
      josephus [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] 1 `shouldBe` [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
      josephus [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] 2 `shouldBe` [2, 4, 6, 8, 10, 3, 7, 1, 9, 5]

    it "works with strings" $ do
      josephus "CodeWars" 4 `shouldBe` "esWoCdra"
