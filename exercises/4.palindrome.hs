module Palindrome where

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = 
    reverse x == x

myAbs :: Integer -> Integer
myAbs r =
    if r < 0 then negate r else r

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = 
    ((snd x, snd y), (fst x, fst y))

f2 :: (a, b) -> (c, d) -> ((b, d), (a, c))
f2 (a,b) (c,d) = ((b, d), (a, c))
