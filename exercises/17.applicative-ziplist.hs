module ApplicativeZipList where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
    Nil
    | Cons a (List a)
    deriving (Eq, Show)

infList :: a -> List a
infList a = Cons a (infList a)

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons h t) = Cons h (take' (n-1) t)

append:: List a -> List a -> List a
append Nil l2 = l2
append (Cons h t) l2 = Cons h (append t l2)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

-- write this one in terms
-- of concat' and fmap
flatMap :: (a -> List b)
            -> List a
            -> List b
flatMap f as = concat' (f <$> as)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons h t) = Cons (f h) (fmap f t)

instance Applicative List where
    pure x = Cons x Nil
    (<*>) _ Nil = Nil
    (<*>) Nil _ = Nil
    (<*>) (Cons f fs) xs = append (fmap f xs) (fs <*> xs)

toList :: [a] -> List a
toList = foldr (\x l -> Cons x l) Nil

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
        as <- listOf arbitrary
        return (toList as)

instance Eq a => EqProp (List a) where 
    (=-=) = eq

-- =================================

newtype ZipList' a =
    ZipList' (List a)
    deriving (Eq, Show)

instance Arbitrary a => Arbitrary (ZipList' a) where
    arbitrary = ZipList' <$> arbitrary

instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys'
        where xs' = let (ZipList' l) = xs
                in take' 3000 l
              ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
    fmap f (ZipList' xs) =
        ZipList' $ fmap f xs

zipAppend:: ZipList' a -> ZipList' a -> ZipList' a
zipAppend (ZipList' l1) (ZipList' l2) = ZipList' (append l1 l2)

-- Function fzip (I invented it :-) )
fzip:: List (a->b) -> List a -> List b
fzip Nil _  = Nil
fzip _ Nil = Nil
fzip (Cons f fs) (Cons a as) = Cons (f a)  (fzip fs as)

-- See comments below:
instance Applicative ZipList' where
    pure x = ZipList' (infList x)  
    (<*>) (ZipList' ff) (ZipList' aa) = ZipList' (fzip ff aa)

lst2 = Cons (1::Int,2::Int,3::Int) Nil

main :: IO ()
main = 
    -- quickBatch (applicative lst2)
    quickBatch (applicative (ZipList' lst2))


-- Lifting x, in the context of a ZipList', means creating an infinite list of x's.
-- e.g. > pure 1 :: ZipList' Int
--    will result in a ZipList containing an infinite list of 1's.
-- e.g. > ii =pure id :: ZipList' (a->a)
--      > :t ii 
--      > ii :: ZipList' (a -> a)
--
-- See history of file for sequence of increasingly better attempts to express (<*>)
