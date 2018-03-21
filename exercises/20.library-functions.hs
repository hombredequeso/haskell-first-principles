module LibraryFunctions20 where

import Data.Monoid -- (Monoid, mempty, mappend, Sum(..), getSum, Product(..), getProduct)

-- Implement the functions in terms of foldMap or foldr from Foldable,

-- 1.
sum' :: (Foldable t, Num a) => t a -> a
sum' = foldr (+) 0


sum'' :: (Foldable t, Num a) => t a -> a
-- sum'' ta = getSum $ foldMap (\x -> Sum x) ta
sum'' = getSum . foldMap Sum

-- 2. product :: (Foldable t, Num a) => t a -> a

product' :: (Foldable t, Num a) => t a -> a
product' = foldr (*) 1

product'' :: (Foldable t, Num a) => t a -> a
product''  = getProduct . foldMap Product


-- 3. elem :: (Foldable t, Eq a)
-- => a -> t a -> Bool

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem a = getAny . foldMap (\x -> Any (x == a))


-- 4. minimum :: (Foldable t, Ord a)
-- => t a -> Maybe a
--

ff :: (a -> a -> a) -> a -> Maybe a -> Maybe a
ff f a Nothing = Just a
ff f a (Just a') = Just (f a a')

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr (ff min) Nothing

-- 5. maximum :: (Foldable t, Ord a)
-- => t a -> Maybe a

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = foldr (ff max) Nothing

-- 6. null :: (Foldable t) => t a -> Bool

null :: (Foldable t) => t a -> Bool
null = foldr (\x acc -> False) True

-- 7. length :: (Foldable t) => t a -> Int
length :: (Foldable t) => t a -> Int
-- length ta = foldr (\x acc -> acc + 1 ) 0 ta
length ta = foldr (const (1 +) ) 0 ta

-- 8. Some say this is all Foldable amounts to.
-- toList :: (Foldable t) => t a -> [a]

toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

-- 9. Hint: use foldMap.
-- -- | Combine the elements
-- -- of a structure using a monoid.
-- fold :: (Foldable t, Monoid m) => t m -> m

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

-- 10. Define foldMap in terms of foldr.
-- foldMap :: (Foldable t, Monoid m)
-- => (a -> m) -> t a -> m

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' a2m = foldr (mappend . a2m ) mempty

