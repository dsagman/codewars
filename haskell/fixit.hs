{-# LANGUAGE FlexibleContexts #-}
-- The fix function can be defined as:

-- fix :: (a -> a) -> a
-- fix f = let x = f x in x
-- If we regard this as a language primitive, any recursive function can be written without using recursion.
-- Write foldr' and reverse' as non-recursive functions that can be 'fixed' to foldr and reverse as follows:

-- foldr = fix foldr'
-- reverse = fix reverse'
-- For a more detailed explanation of the fix function, see http://en.wikipedia.org/wiki/Fixed-point_combinator

-- Note: foldr is lazy, so your foldr' should be lazy too.
-- Also, your foldr' need only work on lists - it does not have to work for an arbitrary Foldable functor.
import Control.Monad
import Data.Function
import Test.Hspec
-- import Test.Hspec.Codewars
import Text.Printf
import Prelude hiding (reverse, foldr)


reverse' :: ([a] -> [a]) -> [a] -> [a]
reverse' f a
  | null a        = []
  | otherwise     = f xs ++ [x]
  where (x:xs) = a


foldr' :: ((a -> t1 -> t2) -> t2 -> [a] -> t1) -> (a -> t1 -> t2) -> t2 -> [a] -> t2
foldr' f g a b
  | null b        = a
  | otherwise     = g x (f g a xs)
  where (x:xs) = b


fixFoldr :: (a -> t2 -> t2) -> t2 -> [a] -> t2
fixFoldr = fix foldr'

fixReverse :: [a] -> [a]
fixReverse = fix reverse'

spec :: Spec
spec = do
  describe (show input1) $ do
    it (printf "sum of %s should return %s" (show input1) (show expected1))
    $ do fixFoldr (+) 0 input1 `shouldBe` expected1
  -- describe "Lazy" $ do
  --   it (printf "foldr should allow lazy evaluation")
  --   $ do take 6 (fixFoldr (\ a b -> (2 * a):b) [] input3) `shouldBe` expected3
  describe (show input1) $ do
    it (printf "reverse of %s is %s" (show input1) (show expected2))
    $ do fixReverse input1 `shouldBe` expected2
  describe (show input4) $ do
    it (printf "reverse of %s is %s" (show input4) (show expected4))
    $ do fixReverse input4 `shouldBe` expected4
  -- describe ("Builtins") $ do
  --   it "should not be using reverse or foldr"
  --   $ do solutionShouldHideAll [ FromModule "Prelude" "reverse"
  --               , FromModule "Prelude" "foldr" ]
  where
    input1 = [2,3,5,7,11]
    input3 = [0..]
    expected1 = 28
    expected2 = [11,7,5,3,2]
    expected3 = [0,2,4,6,8,10]
    input4 = "Reverse"
    expected4 = "esreveR"


-- fact :: Integer -> Integer
-- fact = fix factStep

-- factStep :: (Eq p, Num p) => (p -> p) -> p -> p
-- factStep f n = if n==0 then 1 else n * f (n - 1)

-- fib :: Integer -> Integer
-- fib = fix fibStep
-- fibStep :: (Eq a, Num a, Num p) => (a -> p) -> a -> p
-- fibStep f n
--   | n == 0 = 0
--   | (n-1) == 0 = 1
--   | otherwise = f (n-1) + f (n-2)