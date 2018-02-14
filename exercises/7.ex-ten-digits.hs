module TenDigitsExercise where

tensDigit :: Integral a => a -> a
tensDigit x = d
        where 
            (xLast, _) = x `divMod` 10
            (_,d) = xLast `divMod` 10

 

hundredsDigit :: Integral a => a -> a
hundredsDigit x = d
        where 
            (xLast, _) = x `divMod` 100
            (_,d) = xLast `divMod` 100

foldBool :: a -> a -> Bool -> a
foldBool x y b =
    case b of
      False -> x
      True -> y

foldBoolG :: a -> a -> Bool -> a
foldBoolG x y b 
    | b = y
    | otherwise = x

