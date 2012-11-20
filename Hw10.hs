-- Art of Recursion PS 10
-- Author: Kyle Hardgrave (kyleh@seas)

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

factTR :: Int -> Int
factTR r 0 = r
factTR r n = factTR (n*r) (n-1)

-- | 1. `fact` defined in terms of `factTR`
fact' :: Int -> Int
fact' = factTR 1


-- | 2. Tail-recursive version of `reverse`
-- reverse []     = []
-- reverse (x:xs) = reverse xs ++ [x]
reverse :: [a] -> [a]
reverse = aux [] where
  aux l (x:xs) = aux (x:l) xs
  aux l []     = l