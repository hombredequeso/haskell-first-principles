module MaybeMonoid where

import Data.Monoid
import Test.QuickCheck

-- Monoid law tests:
monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c =
    (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = 
    (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = 
    (a <> mempty) == a

data Optional a =
    Nada
    | Only a
    deriving (Eq, Show)

newtype First' a =
    First' { getFirst' :: Optional a }
    deriving (Eq, Show)

instance Monoid (First' a) where
    mempty = First' {getFirst' = Nada}
    mappend = mGetFirst2

-- mappend = mGetFirst 
mGetFirst :: First' a -> First' a -> First' a
mGetFirst x y = case x of
                  (First' (Only _)) -> x
                  otherwise -> y

-- mappend = mGetFirst2
mGetFirst2 :: First' a -> First' a -> First' a
mGetFirst2 x@(First' (Only _)) _ = x
mGetFirst2 _ y = y


type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = do
        aa <- arbitrary
        let o1 = First' { getFirst' = Only aa }
        let o2 = First' { getFirst' = Nada }
        oneof [return o1, return o2]




main :: IO ()
main = do
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: FstId)
    quickCheck (monoidRightIdentity :: FstId)



-------------------------------------------------------------------------------
-- OUTTAKES: 

-- Generate the arbitrary:
-- optionalGen :: (Arbitrary a) => Gen (Optional a)
-- optionalGen = do
--                 a' <- arbitrary
--                 oneof [
--                         return $ Nada,
--                         return $ Only a'
--                       ]
-- 
-- firstGen :: (Arbitrary a) => Gen (First' a)
-- firstGen = do
--                 x <- arbitrary
--                 let o = Only x 
--                 let f = First' o
--                 let n = First' Nada
--                 elements [n, f]
--                 -- return (First' o)


