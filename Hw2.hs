-- Art of Recursion PS 2 ======================================================
-- Author: Kyle Hardgrave (kyleh@seas)

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where
import Prelude
import Test.HUnit


-- Problem 8 ------------------------------------------------------------------
-- Suppose we wish to count how many ways there are to tile a 1×n 
-- rectangle using 1×1, 1×2, 1×3, and 1×4b locks. (The order of the
-- tiles matters.) For example, there are 29 ways to tile a 1×6
-- rectangle, as shown in Figure 1.
-- Let t(n) denote the number of ways to tile a 1×n rectangle using
-- blocks of length 1, 2, 3, and 4. For example, t(6) = 29.
t :: Int -> Int
t n 
  | n <= 0    = 0
  | n == 1    = 1
  | otherwise = t1 + t2 + t3 + t4 where
    t1 = (t (n-1)) + 1
    t2 = if n > (t (n-2)) + 1 
    t3 = (t (n-3)) + 1         (t (n-4)) + 

test8 :: Test
test8 = TestList [
  "t(6) = 29" ~: t 6 ~?= 29,
  "t(1) = 1" ~: t 1 ~?= 1,
  "t(2) = 2" ~: t 2 ~?= 2,
  "t(3) = 4" ~: t 3 ~?= 4
  ]



-- Tests
doTests :: IO ()
doTests = do
  _ <- runTestTT $ TestList [ test8 ]
  return ()

main :: IO ()
main = do
  doTests
  return ()
