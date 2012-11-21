-- Art of Recursion PS 10
-- Author: Kyle Hardgrave (kyleh@seas)

{-# LANGUAGE GADTs #-}
import Test.QuickCheck

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
-- fib 0 = 0
-- fib 1 = 1
-- fib n = fib (n-1) + fib (n-2)
fib :: Int -> Int
fib = aux 0 1 where
  aux a b 0 = a
  aux a b 1 = b
  aux a b 2 = a + b
  aux a b c = aux b (a+b) (c-1)


-- | 4. Convert `factTR` and `fib` to iterative programs.
{-
def fact(n):
  r = 1
  while n > 0:
    n -= 1
    r = r * n
  return r

def fib(n):
  a, b = 0, 1
  if n == 0:
    return a
  if n == 1:
    return b
  while n > 2:
    n -= 1
    a, b = b, a+b
  return a + b
-}

factCPS :: (Int -> Int) -> Int -> Int
factCPS k 0 = k 1
factCPS k n = factCPS (k . (\r -> n*r)) (n-1)


-- | 5. `fact` defined in terms of `factCPS`
fact2 :: Int -> Int
fact2 = factCPS (*1)


-- | 6. `map` using CPS
-- map _ []     = []
-- map f (x:xs) = f x : map f xs
mapCps :: ([b] -> [b]) -> (a -> b) -> [a] -> [b]
mapCps k _ []     = k []
mapCps k f (x:xs) = mapCps (k . (\l -> ((f x) : l))) f xs

map' :: (a -> b) -> [a] -> [b]
map' f = mapCps id f


-- | 7. Fibonacci using CPS

-- TODO
--fib' :: Int -> Int
--fib' = fibCps

--fibCps :: (Int -> Int -> Int) -> Int -> Int
--fibCps k n = fibCps (k . (\a b -> a + b)) (n-1)
--fibCps k 1 = k 1 0
--fibCps k 0 = k 0

data Direction i o where
  Down :: i -> Direction i o
  Up   :: o -> Direction i o
  
data FactNode where
  -- what to multiply by after finishing the recursion
  FN :: Integer -> FactNode
  
type FactIn  = Integer
type FactOut = Integer

factT :: Direction FactIn FactOut -> [FactNode] -> FactOut
factT (Down 0) stack = factT (Up 1) stack
factT (Down n) stack = factT (Down (n-1)) (FN n : stack)
factT (Up res) (FN n : stack) = factT (Up (n * res)) stack
factT (Up res) []             = res


type FibIn = Integer
type FibOut = Integer

data FibNode where
  FibL :: FibIn  -> FibNode
  FibR :: FibOut -> FibNode

fibT :: Direction FibIn FibOut -> [FibNode] -> Integer
fibT (Down 0) stack               = fibT (Up 0) stack
fibT (Down 1) stack               = fibT (Up 1) stack
fibT (Down n) stack               = fibT (Down (n-1)) (FibL (n-2) : stack)
fibT (Up res) (FibL n2 : stack)   = fibT (Down n2) (FibR res : stack)
fibT (Up res) (FibR res1 : stack) = fibT (Up (res1 + res))  stack
fibT (Up res) []                  = res

-- | 8. Define `fib` in terms of `fibT`
fib2 :: Integer -> Integer
fib2 n = fibT (Down n) []


h :: Integer -> Integer
h 0 = 1
h n | odd n  = h (n `div` 2)
    | even n = h (n `div` 2) + h ((n `div` 2) - 1)

-- | 9. Tail-recursive version of `h`
type HIn = Integer
type HOut = Integer

data HNode where
  HO :: HIn  -> HNode
  HE :: HOut -> HNode
  
half :: Integer -> Integer
half n = n `div` 2

hT :: Direction HIn HOut -> [HNode] -> Integer
hT (Down 0) s      = hT (Up 1) s
hT (Down n) s      | odd n  = hT (Down $ half n) s
                   | even n = hT (Down $ half n) (HE ((half n) - 1) : s)
hT (Up r) (HE n:s) = hT (Down n) (HO r : s)
hT (Up r) (HO n:s) = hT (Up $ r + n) s
hT (Up r) []       = r

h' :: Integer -> Integer
h' n = hT (Down n) []


data BTree a = Empty | Branch (BTree a) a (BTree a)

foldBTree :: c -> (c -> a -> c -> c) -> BTree a -> c
foldBTree e _ Empty          = e
foldBTree e b (Branch l a r) = b (foldBTree e b l) a (foldBTree e b r)

-- | 10. Tail-recursive version of foldBTree
-- type TIn  = BTree a
-- type TOut = b
-- 
-- data TNode where
--   TL :: a -> TIn -> TNode
--   TR :: TOut -> a -> TNode
--   
-- foldT :: b -> (b -> a -> b -> b) -> Direction TIn TOut -> [TNode] -> TOut
-- foldT e f (Down Empty) s        = foldT e f (Up e)         s
-- foldT e f (Down Branch l a r) s = foldT e f (Down l)       (TL a r : s)
-- foldT e f (Up l) (TL a r : s)   = foldT e f (Down r)       (TR l a : s)
-- foldT e f (Up r) (TR l a : s)   = foldT e f (Up (f l a r)) s
-- foldT e f (Up t) []             = t
-- 
-- foldBTree' :: c -> (c -> a -> c -> c) -> BTree a -> c
-- foldBTree' e f t = foldT e f (Down t) []