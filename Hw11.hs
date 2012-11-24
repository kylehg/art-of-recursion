-- Art of Recursion PS 11
-- Author: Kyle Hardgrave (kyleh@seas)

import Data.Array
import Data.Ratio
import Test.HUnit



data BTree a = E | B a (BTree a) (BTree a)
             deriving (Show, Eq)

t1 :: BTree Integer
t1 = B 4 (B 2 (B 1 E E) (B 3 E E)) (B 6 (B 5 E E) (B 7 E E))


-- | 1. The Calkin-Wilf tree.
cw :: BTree Rational
cw = cw' 1 1 where
  cw' a b = B (a % b) (cw' a (a+b)) (cw' (a+b) b)


-- | 2. A list of the elements of a BTree in top-bottom, left-right order.
flatten :: BTree a -> [a]
flatten (B a l r) = flatten' [B a l r] where
  flatten' ((B a l r):ts) = a : flatten' (ts ++ [l, r])
  flatten' (E:ts)         = flatten' (ts ++ [l, r])
  flatten' _              = []
-- TODO: Possible without concatenation?

test2 :: Test
test2 = (take 15 $ flatten cw) ~?= [1%1, 1%2, 2%1, 1%3, 3%2, 2%3, 3%1,
                                    1%4, 4%3, 3%5, 5%2, 2%5, 5%3, 3%4, 4%1]

-- Psuedocode
{-
maxRoute :: [[Integer]] -> Integer
maxRoute = undefined

triangle = [[3],
            [7, 4],
            [2, 4, 6],
            [8, 5, 9, 3]]
toListOfArrays :: [[a]] -> [Array Integer Integer]
toListOfArrays ll =
test3 = maxRoute triangle ~?= 23
-}


doTests :: IO ()
doTests = do
  _ <- runTestTT $ TestList [test2]
  return ()

main :: IO ()
main = doTests