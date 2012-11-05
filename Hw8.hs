-- Art of Recursion PS 8
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


-- | 6. Mergesort: sort a list.
mergesort :: Ord a => [a] -> [a]
mergesort []  = []
mergesort [a] = [a]
mergesort as  = merge (mergesort l') (mergesort r') where
  (l', r') = splitAt ((length as) `div` 2) as
  merge [] r                = r
  merge l []                = l
  merge l@(l0:ls) r@(r0:rs) | l0 <= r0  = l0 : merge ls r
                            | otherwise = r0 : merge l rs
