import Test.Hspec

positiveSum :: [Int] -> Int
positiveSum = sum . filter (>0)




-- spec = hspec $ do
--   describe "positiveSum" $ do
--     it "works for some examples" $ do
--       positiveSum [1, 2, 3, 4, 5] `shouldBe` 15
--       positiveSum [1, -2, 3, 4, 5] `shouldBe` 13
--       positiveSum [-1, 2, 3, 4, -5] `shouldBe` 9
    
--     it "returns 0 if array is empty" $ do
--       positiveSum [] `shouldBe` 0
    
--     it "returns 0 if all elements are negative" $ do
--       positiveSum [-1, -2, -3, -4, -5] `shouldBe` 0


