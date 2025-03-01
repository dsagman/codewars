maxList :: [Integer] -> Integer
maxList xs = aux xs 0
    where 
        aux [ ] a = a
        aux (x:xs) a = aux xs $ max x a
-- maxList xs = aux xs 0
--     where 
--         aux [ ] a = a
--         aux (x:xs) a = aux xs $ max x a