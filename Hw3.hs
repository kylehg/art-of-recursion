-- Art of Recursion PS 3 ======================================================
-- Author: Kyle Hardgrave (kyleh@seas)

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where
import Prelude
import Test.HUnit

--"Elements of type Game are intended to represent certain types
-- of two-player games. At the root of every Game is a Node containing
-- a ChoiceList, which is a list of Choices. Each Choice represents
-- some move or choice that the first player could make on their
-- turn, and contains another Game—namely, the game which results
-- if that choice is made. It is then the second player’s turn to
-- make a move in that Game, and so on. A player loses when it is
-- their turn but they have no available moves, i.e. there is an
-- empty ChoiceList, represented by Lose." --byorgey
data ChoiceList = Lose
                | Choice Game ChoiceList
                deriving (Show, Eq)

data Game = Node ChoiceList
          deriving (Show, Eq)


-- Problem 11 -----------------------------------------------------------------
game1 :: Game
game1 = Node Lose

game2 :: Game
game2 = Node (Choice (Node Lose) Lose)

game3 :: Game
game3 = Node (Choice (Node Lose) (Choice (Node Lose) Lose))


-- Problem 12 -----------------------------------------------------------------
-- | A function that takes any Game as input and computes
--   whether the first player to move can force a win.
win :: Game -> Bool
-- Base: The player facing this choice cannot win.
win (Node Lose) = False 
-- Inductive: Either the game that's left is destined to lose or
-- another choice in the ChoiceList is destined to win.
win (Node (Choice g cl)) = lose g || win (Node cl)


-- | A function that takes a Game as input and computes whether the
--   first player to move is doomed to lose (assuming their opponent
--   plays perfectly). Can be defined in terms of `win` (and vice versa).
lose :: Game -> Bool
-- Base: The player facing this choice _must_ lose.
lose (Node Lose) = True
-- Inductive: Either the game that's left is destined to win or
-- another choice in the ChoiceList is destined to lose.
lose (Node (Choice g cl)) = win g || lose (Node cl)


test12 :: Test
test12 = TestList [
  "win false example" ~: win game1 ~?= False,
  "win true example" ~: win game2 ~?= True,
  "lose true example" ~: lose game1 ~?= True,
  "lose false example" ~: lose game2 ~?= True,
  "win true example 2" ~: win game3 ~?= True,
  "lose true example 2" ~: lose game3 ~?= True
  ]


-- Problem 13 -----------------------------------------------------------------
--"The game of two-pile nim is played as follows. There are two
-- sep-arate piles of counters. Two players alternate turns. On a
-- player’s turn, she may take any number of counters from one of
-- the two piles (but not both); but she must take at least one
-- counter. The player who takes the last counter wins. (Put differently,
-- the first player unable to move -- due to all the counters being
-- gone -- loses.)" --byorgey

-- | A function that takes as input the sizes x and y of the two piles,
--   and outputs a Game representing the game of two-pile nim.
nim :: Int -> Int -> Game
nim x y = Node (choices x y) where
  choices :: Int -> Int -> ChoiceList
  choices
    -- Aside from finding it hard to reason about Haskell, this
    -- algorithm appears to me to be O(2^n). However, I believe with
    -- dyanmic programming (i.e., storing our `choices` results in a
    -- memoized array) we can make it polynomial. (In principle O(n^2)
    -- should be possible, though if weren't careful it would probably be
    -- O(n^3) because of the weird `nim` outer function [which I *do*
    -- think is necessary].)
    | x == 0 && y == 0 = Lose
    | x == 0           = Choice (nim x (y-1)) (choices x (y-1))
    | y == 0           = Choice (nim (x-1) y) (choices (x-1) y)
    | otherwise        = Choice (nim (x-1) y) 
                         (Choice (nim x (y-1)) (choices x (y-1)))


-- Tests
doTests :: IO ()
doTests = do
  _ <- runTestTT $ TestList [ test12 ]
  return ()
  
main :: IO ()
main = do
  doTests
  return ()
  
  
  -- Problem 10 (out of order)
-- First, we write a function that exemplifies induction on a list,
-- for clarity / consistency.
--trueForList :: (a -> Bool) -> [a] -> Bool
--trueForList pred (x:xs) = pred x && trueForList pred xs
--trueForList [] = False -- For Haskell purposes, we assume the empty
--                       -- list isn't empty.

-- Now we show an equivalent statement for games.
--trueForGame :: (ChoiceList -> Bool) -> Game -> Bool
--trueForGame Node cl = -- TODO


