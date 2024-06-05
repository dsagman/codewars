import Test.Hspec
import Control.Monad



smallestPossibleSum :: (Integral a) => [a] -> a
smallestPossibleSum xs =  fromIntegral (length xs) * foldr1 gcd xs

smallestPossibleSum'' :: (Integral a) => [a] -> a
smallestPossibleSum'' = ap ((*) . fromIntegral . length) (foldr1 gcd)

spec = hspec $ do
  describe "smallest possible sum" $ do
    it "example tests" $ do
      smallestPossibleSum [6,9,21] `shouldBe` 9
      smallestPossibleSum [1,21,55] `shouldBe` 3
      smallestPossibleSum [3,13,23,7,83] `shouldBe` 5
      smallestPossibleSum [4,16,24] `shouldBe` 12
      smallestPossibleSum [30,12] `shouldBe` 12
      smallestPossibleSum [60,12,96,48,60,24,72,36,72,72,48] `shouldBe` 132
      smallestPossibleSum [71,71,71,71,71,71,71,71,71,71,71,71,71] `shouldBe` 923
      smallestPossibleSum [11,22] `shouldBe` 22
      smallestPossibleSum [9] `shouldBe` 9