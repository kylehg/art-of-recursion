{-# LANGUAGE GADTs #-}

-- Recursive functions --------------------------------------------------------
-- | 1.
h :: Int -> Int
h 0 = 1
h n = if even n then h (k-1) + h k else h k where k = (n `div` 2)

-- The apparent runtime of h() would be O(2^n), since every even call
-- makes two recursive calls. However, caclulating the amortized runtime
-- would likely provide a lower bound, especially remembering from the
-- midterm that h() often got to a base case pretty quickly.

-- | 2.
-- TODO

-- | 3.
allHB :: Integer -> [[Int]]
allHB n
  | n <= 0    = [[]]
  | even n    = (consAll 0 $ allHB k) ++ (consAll 2 $ allHB $ k - 1)
  | otherwise = consAll 1 $ allHB k
  where k = n `div` 2
        consAll m = map $ (:) m
-- TODO: Fix 0 case

