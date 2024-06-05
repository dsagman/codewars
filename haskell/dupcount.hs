{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleContexts #-}
import Data.List (nub, (\\))
import Data.Char ( toLower )

duplicateCount :: String -> Int
duplicateCount xs = length $ filter (>1) counts
    where
          counts = [length (filter (==x) ys) | x <- nub ys]
          ys = map toLower xs




duplicateCount' :: String -> Int
duplicateCount' = length . (fmap nub <$> (\\) <*> nub) . map toLower

b :: String -> Int
b = length . nub . ((\\) <*> nub). map toLower


a2 xs = do
    let ys = map toLower xs
    x <- nub ys
    let a = filter (==x) ys
    let b = length a
    return b


t = "hello"
t2 = "hello World"

t3 = "KJPMgOiy2Ciyz75g55"
-- t3 should be 4