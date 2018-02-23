module BadMonoid where

-- To get this working, it was necessary to install the checkers module using cabal.
-- On my system this involved:
-- > cabal update
-- > cabal install checkers
-- .
-- It is possible to check the packages that are available to ghci using:
-- > cabal list --installed
-- Since that is likely to be a very long list, you may want to:
-- > cabal list --installed | grep checkers
-- And if you need information about a module, try:
-- > cabal info checkers
--

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Bull =
    Fools
    | Twoo
    deriving (Eq, Show)

instance Arbitrary Bull where
    arbitrary =
        frequency [ (1, return Fools)
                  , (1, return Twoo) ]

instance Monoid Bull where
    mempty = Fools
    mappend _ _ = Fools

instance EqProp Bull where (=-=) = eq

main :: IO ()
main = quickBatch (monoid Twoo)
