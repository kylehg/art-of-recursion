-- Art of Recursion PS 7
-- Author: Kyle Hardgrave (kyleh@seas)

{-# LANGUAGE GADTs #-}
import Test.HUnit


data BST a where
  Empty :: BST a
  Node  :: BST a -> a -> BST a -> BST a

bstFold :: r -> (r -> a -> r -> r) -> BST a -> r
bstFold e _ Empty        = e
bstFold e f (Node l a r) = f (bstFold e f l) a (bstFold e f r)

-- | 2. Insert a new value into a binary search tree.
insert :: Ord a => a -> BST a -> BST a
insert a Empty        = Node Empty a Empty
insert a (Node l x r) | a < x     = Node (insert a l) x r
                      | a > x     = Node l x (insert a r)
                      | otherwise = Node l x r

-- | 3. Insert a new value into a binary search tree using `bstFold`.
--insert' :: Ord a => a -> BST a -> BST a
--insert' a = bstFold (Node a Empty Empty) f where
--  f l x r = undefined -- TODO

-- | Merge two smaller sorted lists into a single sorted list.
merge :: Ord a => [a] -> [a] -> [a]
merge [] r          = r
merge l []          = l
merge (l:ls) (r:rs) | l <= r    = l : merge ls (r:rs)
                    | otherwise = r : merge (l:ls) rs

testMerge :: Test
testMerge = TestList [
  "testMerge 1" ~: merge [1, 2, 3] [3, 4, 5] ~?= [1, 2, 3, 3, 4, 5],
  "testMerge 2" ~: merge [3, 4, 5] [1, 2, 3] ~?= [1, 2, 3, 3, 4, 5],
  "testMerge 3" ~: merge [6, 4, 2] [3, 5, 1] ~?= [3, 5, 1, 6, 4, 2],
  "testMerge 4" ~: merge [2, 4, 6] [3, 5, 1] ~?= [2, 3, 4, 5, 1, 6]
  ]
  

-- | Given a pivot and a list, split the list into two lists of elements 
-- less-than and greater-than-or-equal-to the pivot, respectively.
split :: Ord a => a -> [a] -> ([a], [a])
split a = foldr f ([], []) where
  f n (l, r) = if n < a then (n:l, r) else (l, n:r) 
  
-- | 6. Mergesort
mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort (a:[]) = [a]
mergesort as = merge (mergesort l) (mergesort r) where
  (l, r) = split (as !! ((length as) `div` 2)) as



doTests :: IO ()
doTests = do
  _ <- runTestTT $ TestList [ testMerge ]
  return ()

main :: IO ()
main = do
  doTests
  return ()