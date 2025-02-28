maxList xs = aux xs x
    where 
        aux [ ] a = a
        aux (x:xs) a = aux xs $ max (x a)