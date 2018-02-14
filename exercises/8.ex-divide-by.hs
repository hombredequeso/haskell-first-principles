{-# LANGUAGE FlexibleContexts #-}

module DivideByEx where

data DividedResult a' =
    Result (a',a')
    | DividedByZero
        deriving Show

negTup1 :: Integral a => (a, a) -> (a, a)
negTup1 (x, y) = (-x, y)

negTup2 :: Integral a => (a, a) -> (a, a)
negTup2 (x, y) = (x, y)

dividedBy :: Integral a => a -> a -> DividedResult a
dividedBy _ 0 = DividedByZero
-- dividedBy num den = Result (go num den 0)
--     where
--         go n d count
--             | n < d = (count, n)
--             | otherwise = go (n - d) d (count + 1)
--


dividedBy num den 
  | (num < 0) && den < 0 = Result (go (negate num) (negate den) 0)
  | (num < 0) = Result (negTup1(go (negate num) den 0))
  | (den < 0) = Result (negTup2(go (negate num) den 0))
  | otherwise = Result (go num den 0)
        where
            go n d count
                | n < d = (count, n)
                | otherwise = go (n - d) d (count + 1)
