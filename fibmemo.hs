{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.State
    ( MonadState(get), State, modify, evalState )
import Data.Maybe ( fromJust )
import qualified Data.Map as M

-- fibonacci :: Int -> Integer
-- fibonacci 0 = 0
-- fibonacci 1 = 1
-- fibonacci n = fibonacci (n-1) + fibonacci (n-2)

fibonacci :: Int -> Integer
fibonacci n = f' 0 1 !! n
    where f' x y = x : f' y (x+y)

f :: Int -> Integer
f n = f' 0 1 !! n

f' :: Integer -> Integer -> [Integer]
f' x y = x : f' y (x+y)

fibSCIP :: Integer -> Integer
fibSCIP = fibIter 1 0

fibIter :: (Eq t1, Num t1, Num t2) => t2 -> t2 -> t1 -> t2
fibIter a b count = if count == 0 then b
      else fibIter (a + b) a (count - 1)

-- version with better formula
--           2       2                          2
-- F     = F     + F     and F   = 2F    F  - F
--  2n+1     n+1    n         2n     n+1  n    n

-- f1 = F(n)
-- f2 = F(n+1)
-- n == 2n or 2n+1

f2 :: Integer -> Integer
f2 n
    | n >= 0     = fib2 n
    | even n    = negate (fib2 (negate n))
    | otherwise = fib2 (negate n)

fib2 :: Integer -> Integer
fib2 0 = 0
fib2 1 = 1
fib2 2 = 1
fib2 n
    | even n = f1 * (2 * f2 - f1)
    | otherwise = f1 * f1 + f2 * f2
    where   k = div n 2
            f1 = fib2 k
            f2 = fib2 (k+1)



-- version with state monad
fibLet :: Int -> State (M.Map Int Integer) Integer
fibLet n = do
   case n of 0 -> return 0
             1 -> return 1
             n -> do
                  mp <- get
                  if M.member n mp
                  then return $ fromJust (M.lookup n mp)
                  else do
                       let s1 = evalState (fibLet (n - 1)) mp
                       let s2 = evalState (fibLet (n - 2)) mp
                       let s3 = s1+s2
                       modify $ M.insert n s3
                       return s3

fibArrow :: Int -> State (M.Map Int Integer) Integer
fibArrow n = do
    case n of 0 -> return 0
              1 -> return 1
              n -> do
                   mp <- get
                   if M.member n mp
                   then return $ fromJust (M.lookup n mp)
                   else do
                        s1 <-fibLet (n - 1)
                        s2 <-fibLet (n - 2)
                        let s3 = s1+s2
                        modify $ M.insert n s3
                        return s3


-- fibArrow :: Int -> State (M.Map Int Integer) Integer
-- fibArrow n = do
--     case n of 0 -> return 0
--               1 -> return 1
--               n -> do
--                    mp <- get
--                    if M.member n mp
--                    then return $ fromJust (M.lookup n mp)
--                    else do
--                         s <- ((+) <$> fibArrow (n - 2)) <*> fibArrow (n - 1)
--                         modify $ M.insert n s
--                         return s

fibonacciLet :: Int -> Integer
fibonacciLet n = evalState (fibLet n) M.empty

fibonacciArrow :: Int -> Integer
fibonacciArrow n = evalState (fibArrow n) M.empty