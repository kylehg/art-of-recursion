-- Art of Recursion PS 10
-- Author: Kyle Hardgrave (kyleh@seas)

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

factTR :: Int -> Int -> Int
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
  aux l _      = l

-- | 3. Tail-recursive version of Fibbonacci
fib' 0 = 0
fib' 1 = 1
fib' n = fib' (n-1) + fib' (n-2)

fib :: Int -> Int
fib = aux 0 1 where
  aux a b 0 = a
  aux a b 1 = b
  aux a b 2 = a + b
  aux a b c = aux b (a+b) (c-1)
