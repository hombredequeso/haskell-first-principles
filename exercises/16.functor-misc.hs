module Functorification where

data Wrapit a = Wrapit a deriving (Eq, Show)

instance Functor Wrapit where
    fmap f (Wrapit a) = Wrapit $ f a

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a $ f b

class Sumthin a where
    s :: a -> a

instance Sumthin (Wrapit a) where
    s x = x

class SumthinB f where
    t :: f a -> f a

instance SumthinB Wrapit where
    t (Wrapit x) = Wrapit x


class Else c where
    e :: b -> f (g a b c)

class Biffy b where
    slayer :: e a b
        -> (a -> c)
        -> (b -> d)
        -> e c d


n = Nothing
w = Just "woohoo"
ave = Just "Ave"

lms = [ave, n, w]

replaceWithP = const 'p'

f1 = fmap replaceWithP
f1Result = f1 lms

f2 = (fmap . fmap) replaceWithP
f2Result = f2 lms

f3 = (fmap . fmap . fmap) replaceWithP
f3Result = f3 lms


