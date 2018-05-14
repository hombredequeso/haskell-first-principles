module OptionalFolding where

data Optional a =
    Nada
  | Yep a
  deriving (Eq, Show)


instance Foldable Optional where
    foldr _ z Nada = z
    foldr f z (Yep x) = f x z

    foldl _ z Nada = z
    foldl f z (Yep x) = f z x

    foldMap _ Nada = mempty
    foldMap f (Yep a) = f a


-- the function foldMap requires that the type of 'a' in 'Optional a' is a monoid, so it can mempty. Note the following usage :


-- *OptionalFolding Data.Monoid> foldMap (+1) (Yep 10)

-- <interactive>:45:1: error:
--     * Ambiguous type variable `a0' arising from a use of `print'
--       prevents the constraint `(Show a0)' from being solved.
--       Probable fix: use a type annotation to specify what `a0' should be.
--       ...
-- *OptionalFolding Data.Monoid>
-- *OptionalFolding Data.Monoid> x = foldMap (+1) (Yep 10)
-- *OptionalFolding Data.Monoid> x::Sum Integer
-- Sum {getSum = 11}
-- *OptionalFolding Data.Monoid> y = foldMap (+1) Nada
-- *OptionalFolding Data.Monoid> foldMap (+1) Nada

-- <interactive>:51:1: error:
--     * Ambiguous type variable `a0' arising from a use of `print'
--       prevents the constraint `(Show a0)' from being solved.
--       Probable fix: use a type annotation to specify what `a0' should be.
--       ...
-- *OptionalFolding Data.Monoid> y :: Sum Integer
-- Sum {getSum = 0}
-- *OptionalFolding Data.Monoid> (foldMap (+1) Nada) :: Sum Integer
-- Sum {getSum = 0}
-- *OptionalFolding Data.Monoid>

