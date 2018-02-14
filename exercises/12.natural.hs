module NaturalNumbers where

-- As natural as any
-- competitive bodybuilder
data Nat =
    Zero
    | Succ Nat
    deriving (Eq, Show)

-- >>> natToInteger Zero
-- 0
-- >>> natToInteger (Succ Zero)
-- 1
-- >>> natToInteger (Succ (Succ Zero))
-- 2

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + (natToInteger n)


-- >>> integerToNat 0
-- Just Zero
-- >>> integerToNat 1
-- Just (Succ Zero)
-- >>> integerToNat 2
-- Just (Succ (Succ Zero))
-- >>> integerToNat (-1)
-- Nothing

integerToNat :: Integer -> Maybe Nat
integerToNat i 
    | i < 0 = Nothing
    | i == 0 = Just Zero
    | otherwise = Just (iToN i)

iToN :: Integer -> Nat
iToN 0 = Zero
iToN i = Succ (iToN (i-1))

iToN2 :: Integer -> Nat
iToN2 i = case i of
            0 -> Zero
            x -> Succ (iToN2 x)



integerToNat :: Integer -> Maybe Nat
integerToNat i 
    | i < 0 = Nothing
    | i == 0 = Just Zero
    | otherwise = Just n 
