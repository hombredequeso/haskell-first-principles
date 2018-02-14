module Unfold where

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : (myIterate f (f a))

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = result where
    y= f b
    result = case y of
               Just (a2,b2) ->  a2: myUnfoldr f b2
               Nothing -> []


betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\x -> Just(x, (f x))) x
