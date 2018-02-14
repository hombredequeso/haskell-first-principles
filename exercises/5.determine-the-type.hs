{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

example = 1

-- a = (* 9) 6
-- b = head [(0,"doge"),(1,"kitteh")]
-- c = head [(0 :: Integer ,"doge"),(1,"kitteh")]
-- d = if False then True else False
-- e = length [1, 2, 3, 4, 5]
-- f = (length [1, 2, 3, 4]) > (length "TACOCAT")


bigNum = (^) 5 $ 10
wahoo = bigNum * 10
 
x = print
y = print "woohoo!"
z = x "hello world"

functionH ::[a] -> a
functionH (x:_) = x

functionC :: Ord a => a -> a -> Bool
functionC x y =
    if (x > y) then True else False

functionS :: (a,b) -> b
functionS (x, y) = y

i :: a -> a
i x = x

c :: a -> b -> a
c x y = x

r :: [a] -> [a]
r x = x

r2 :: [a] -> [a]
r2 x = []

r3 :: [a] -> [a]
r3 x = reverse x

co :: (b -> c) -> (a -> b) -> a -> c
co bToc aTob x = bToc(aTob x) 

