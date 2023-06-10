import Control.Monad ( liftM2 )


-- import Test.Hspec


divide :: Integer -> Bool
divide w = even w && (w > 3)

divide' :: Integer -> Bool
divide' = liftM2 (&&) even (> 3)

-- divide'' :: Integer -> Bool
divide'' :: Integer -> Bool
divide''  =  and <$> sequence [even, (> 3)]



-- spec :: Spec
-- spec = do
--   describe "divide" $ do
--     it "should work for some examples" $ do
--       divide 1 `shouldBe` False
--       divide 2 `shouldBe` False
--       divide 3 `shouldBe` False
--       divide 4 `shouldBe` True
--       divide 5 `shouldBe` False
--       divide 6 `shouldBe` True