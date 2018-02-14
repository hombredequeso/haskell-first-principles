module SumEx where

sumM:: (Eq a, Num a) => a -> a
sumM 0 = 0
sumM x = x + (sumM (x-1))


multM :: (Integral a) => a -> a -> a
multM 0 x = 0
multM 1 x = x
multM m x = x + (multM (m-1) x)

