-- Art of Recursion PS 11
-- Author: Kyle Hardgrave (kyleh@seas)

import Data.Array
import Data.Ratio
import Test.HUnit

import Hw11Triangle -- The data representation of the triangle to solve



-- | A binary tree
data BTree a = E | B a (BTree a) (BTree a)
             deriving (Show, Eq)

-- | Test tree
t1 :: BTree Integer
t1 = B 4 (B 2 (B 1 E E) (B 3 E E)) (B 6 (B 5 E E) (B 7 E E))


-- | 1. The Calkin-Wilf tree, an infinite binary tree of all rational numbers.
cw :: BTree Rational
cw = cw' 1 1 where
  cw' a b = B (a % b) (cw' a (a+b)) (cw' (a+b) b)


-- | 2. A breadth-first traversal of the list of the elements of a BTree.
flatten :: BTree a -> [a]
flatten (B a l r) = flatten' [B a l r] where
  flatten' ((B a l r):ts) = a : flatten' (ts ++ [l, r])
  flatten' (E:ts)         = flatten' (ts ++ [l, r])
  flatten' _              = []
-- TODO: Possible without concatenation? Better queue structure.

test2 :: Test
test2 = (take 15 $ flatten cw) ~?= [1%1, 1%2, 2%1, 1%3, 3%2, 2%3, 3%1,
                                    1%4, 4%3, 3%5, 5%2, 2%5, 5%3, 3%4, 4%1]


-- 3. Solve [Project Euler #67](http://projecteuler.net/problem=67)
-- Algorithm
{-
maxTo[row, i] = max(maxTo[row-1, i-1], maxTo[row-1, i]) + row[i]
maxOfLast = max for i in row of maxTo[lastRow, i]
-}

{-
-- A triangle for testing
triangle = [[3],
            [7, 4],
            [2, 4, 6],
            [8, 5, 9, 3]]
-}

-- | An array of arrays of the values in the triangle
arr :: Array Int (Array Int Int)
arr = listArray (0, length triangle - 1) $ 
      map (\r -> listArray (0, (length r) - 1) r) triangle

-- | The 2D array of max paths for each row of the triangle.
maxPaths :: Array (Int, Int) Int
maxPaths = array ((0, 0), (n, n)) vals where
  vals = [((r, i), newMax r i) | r <- [0..n], i <- [0..r]]
  newMax 0 i = arr!0!i
  newMax r i | i == 0 = arr!r!i + maxPaths!(r-1,i)
             | i < r  = arr!r!i + max (maxPaths!(r-1,i-1)) (maxPaths!(r-1,i))
             | i == r = arr!r!i + maxPaths!(r-1,i-1)
             | otherwise = error "Row was longer than its row index."
  (_, n) = bounds arr

-- | The max path down the triangle (simply max over the last row)
maxPath :: Int
maxPath = maximum [maxPaths!(rn, j) | j <- [j0..jn]] where
  ((_, j0), (rn, jn)) = bounds maxPaths


-- A binary tree where each node has a pointer to its parent
type Parent a = Maybe (PBTree a)
data PBTree a = PE (Parent a)
              | PB a (PBTree a) (PBTree a) (Parent a)
              deriving (Show, Eq)


-- | 4. Turn a normal binary tree into a PBTree with back links from 
-- each node to its parent.
linkParents :: BTree a -> PBTree a
linkParents t = link t Nothing where
  link E p         = PE p
  link (B a l r) p = let t = PB a (link l $ Just t) (link r $ Just t) p in t



doTests :: IO ()
doTests = do
  _ <- runTestTT $ TestList [test2]
  return ()

main :: IO ()
main = doTests