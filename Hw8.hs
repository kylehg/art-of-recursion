-- Art of Recursion PS 7
-- Author: Kyle Hardgrave (kyleh@seas)

{-# LANGUAGE GADTs #-}



data BST a where
  Empty :: BST a
  Node  :: BST a -> a -> BST a -> BST a

bstFold :: b -> (b -> a -> b -> b) -> BST a -> b
bstFold b _ Empty        = b
bstFold b f (Node l a r) = f (bstFold b f l) a (bstFold b f r)


-- | 2. Insert a new value into a binary search tree.
insert :: Ord a => a -> BST a -> BST a
insert a Empty        = Node Empty a Empty
insert a (Node l x r) | a < x     = Node (insert a l) x r
                      | a > x     = Node l x (insert a r)
                      | otherwise = Node l x r


-- | 6. Mergesort
mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [a] = [a]
mergesort as = merge (mergesort l') (mergesort r') where
  (l', r') = splitAt ((length as) `div` 2) as
  merge [] r          = r
  merge l []          = l
  merge (l:ls) (r:rs) | l <= r    = l : merge ls (r:rs)
                      | otherwise = r : merge (l:ls) rs
