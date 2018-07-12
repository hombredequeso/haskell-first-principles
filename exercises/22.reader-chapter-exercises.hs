module ReaderPractice where

import Control.Applicative
import Data.Maybe

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]


-- using:
-- lookup :: Eq a => a -> [(a, b)] -> Maybe b

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = lookup 3 (zip x y)

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = lookup 6 (zip y z)

-- it's also nice to have one that
-- will return Nothing, like this one
-- zip x and y using 4 as the lookup key
zs :: Maybe Integer
zs = lookup 4 $ zip x y

-- now zip x and z using a
-- variable lookup key
z' :: Integer -> Maybe Integer
z' n = lookup n (zip x z)

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 i = (,)  ( Just i ) ( Just i )

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' a2b2c (a, b) = a2b2c a b
-- that first argument is a function
-- in this case, we want it to be addition
-- summed is uncurry with addition as
-- the first argument

summed :: Num c => (c, c) -> c
summed = uncurry' (+)

bolt :: Integer -> Bool
-- use &&, >3, <8
bolt' i = (i > 3) && (i < 8)
bolt'' i = ((>) i 3) && ((<) i 8)
bolt''' i = (flip (>) 3 i) && (flip (<) 8 i)
-- the t is going to be:  (->) i
bolt = liftA2 (&&) (>3) (<8)
boltA = liftA2 (&&) (flip (>) 3 ) (flip (<) 8 )
boltB = liftA2 (&&) (\x -> x > 3) (\x -> x < 8 )

fromMaybe' :: a -> Maybe a -> a
fromMaybe' _ (Just a) = a
fromMaybe' a (Nothing) = a

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

sequAAnd :: Integral a => a -> Bool
sequAAnd m = foldr ( && ) True (sequA m)

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

--2. apply sequA to s'; you’ll need fromMaybe.
ex2 =  fromMaybe' [True] (sequA <$> s') 

-- 3.  apply bolt to ys; you’ll need fromMaybe.
ex3 = fromMaybe' True (bolt <*> ys)

main :: IO ()
main = do
        print $ sequenceA [Just 3, Just 2, Just 1]
        print $ sequenceA [x, y]
        print $ sequenceA [xs, ys]
        print $ summed <$> ((,) <$> xs <*> ys)
        print $ fmap summed ((,) <$> xs <*> zs)
        print $ bolt 7
        print $ fmap bolt z

        -- > :t (sequenceA [(>3), (<8), even])
        -- (sequenceA [(>3), (<8), even]) :: Integral a => a -> [Bool]
        -- > :t ([(>3), (<8), even])
        -- ([(>3), (<8), even]) :: Integral a => [a -> Bool]
        -- Hence, go from a list of functions that take an int and produce a bool,
        -- to a function taking an int resulting in a list of bools.
        -- Since function is applicative (Reader), 
        -- it takes the (a ->) from inside, and puts it outside the list.
        print $ sequenceA [(>3), (<8), even] 7
        print $ sequAAnd 7

