maxList :: (Num t1, Ord t1) => [t1] -> t1
maxList xs = aux xs 0
    where 
        aux [] a = a
        aux (x:xs) a = aux xs $ max x a


maxListCPS :: (Num t1, Ord t1) => [t1] -> (t1 -> t2) -> t2
maxListCPS xs k = aux xs k
    where
        aux [] k = k 0
        aux (x:xs)  k = aux xs $ \v -> k (max x v)



dotProduct :: Num a => [a] -> [a] -> a
dotProduct _ [] = 0
dotProduct [] _ = 0
dotProduct (x : xs) (y : ys) = (x * y) + dotProduct xs ys

dotProductTR :: Num a => [a] -> [a] -> a
dotProductTR x y = aux x y 0
    where 
        aux [] _ a = a
        aux _ [] a = a
        aux (x : xs) (y : ys) a = aux xs ys (a + (x * y))

dotProductCPS :: Num t1 => [t1] -> [t1] -> (t1 -> t2) -> t2
dotProductCPS x y k = aux x y k
    where
        aux [] _ k = k 0
        aux _ [] k = k 0
        aux (x : xs) (y : ys) k = aux xs ys $ \v -> k (x * y + v)

fact :: (Eq t, Num t) => t -> t
fact n = aux n 1
    where
        aux 0 a = a
        aux n a = aux (n - 1) (a * n)

factCPS :: (Eq t1, Num t1) => t1 -> (t1 -> t2) -> t2
factCPS n k = aux n k
    where
        aux 0 k = k 1
        aux n k = aux (n - 1) $ \v -> k (v * n)
