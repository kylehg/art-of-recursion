> {-# LANGUAGE GADTs #-}

Recursive functions
-------------------

1.

> h :: Int -> Int
> h 0 = 1
> h n = if even n then h (k-1) + h k else h k where k = (n `div` 2)

The apparent runtime of h() would be O(2^n), since every even call
makes two recursive calls. However, caclulating the amortized runtime
would likely provide a lower bound, especially remembering from the
midterm that h() often got to a base case pretty quickly.

2.
TODO

-- | 3. Generate all the distinct hyperbinary representations of a
-- given number, with resulting digits in reverse order.

> allHB :: Integer -> [[Int]]
> allHB n
>   | n <= 0    = [[]]
>   | even n    = (consAll 0 k) ++ (consAll 2 $ k - 1)
>   | otherwise = consAll 1 k
>   where k = n `div` 2
>         consAll m l = map ((:) m) (allHB l)
> -- TODO: Fix 0 case

4.
TODO


Structural recursion and folds
------------------------------

> data BTree a where
>   Leaf   :: a -> BTree a
>   Branch :: BTree a -> BTree a -> BTree a
>   deriving Show

> tree1 = Branch (Branch (Leaf 1) (Leaf 2))
>                (Branch (Branch (Leaf 3) (Leaf 4))
>                        (Leaf 5))

> tree2 = Branch (Branch (Branch (Leaf 1) (Leaf 2))
>                        (Branch (Leaf 3) (Leaf 4)))
>                (Branch (Branch (Branch (Leaf 5) (Leaf 6))
>                                (Leaf 8))
>                        (Leaf 9))

> tree3 = Branch (Branch (Branch (Leaf 1) (Leaf 2))
>                        (Leaf 3))
>                (Branch (Leaf 4)
>                        (Branch (Leaf 5) (Leaf 6)))

-- | 5. Fold for BTree.

> foldt :: (a -> b) -> (b -> b -> b) -> BTree a -> b
> foldt b _ (Leaf x) = b x
> foldt b f (Branch t1 t2) = f (foldt b f t1) (foldt b f t2)

-- | 6(a). Count the number of leaves in a tree.

> leaves :: BTree a -> Int
> leaves = foldt (\_ -> 1) (+)

-- | 6(b). Compute the length of the longest path from the root to any
-- leaf. Note that `depth (Leaf x) = 0`.

> depth :: BTree a -> Int
> depth = foldt (\_ -> 0) $ \x y -> 1 + max x y

-- | 6(c). Transform a tree full of a’s into a tree full of b’s by
-- applying the given function in every Leaf.

> treeMap :: (a -> b) -> BTree a -> BTree b
> treeMap f = foldt (Leaf . f) Branch

-- | 6(d). Compute the _width_ of a tree, defined as the length of the
-- longest path between any two leaves.

> treeWidth :: BTree a -> Int
> treeWidth t = snd $ foldt (\_ -> (0, 0)) f t where
>   f (d1, w1) (d2, w2) = (1 + max d1 d2, maximum [(2 + d1 + d2), w1, w2])

This looks god-awful, and in truth we could make it prettier by just
calling `depth` instead of keeping track of `d1` and `d2` in a tuple.
That's much less efficient, though.