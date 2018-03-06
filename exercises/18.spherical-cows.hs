module SphericalCows where

import Control.Applicative (liftA3)
import Control.Monad (join)
-- Monad is in GHC.Base
--
-- instance Monad Maybe where
--     return x = Just x
--     (Just x) >>= k = k x
--     Nothing >>= _ = Nothing

data Cow = Cow {
    name :: String
    , age :: Int
    , weight :: Int
    } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

-- if Cow's name is Bess, must be under 500
weightCheck :: Cow -> Maybe Cow
weightCheck c =
    let w = weight c
        n = name c
    in if n == "Bess" && w > 499
        then Nothing
        else Just c

mkSphericalCow'' :: String
                    -> Int
                    -> Int
                    -> Maybe Cow
mkSphericalCow'' name' age' weight' =
    noEmpty name' >>=
    \nammy ->
    noNegative age' >>=
    \agey ->
    noNegative weight' >>=
    \weighty ->
    weightCheck (Cow nammy agey weighty)


cowL = liftA3 Cow

mkSpericalCowApplicative :: String -> Int -> Int -> Maybe Cow
mkSpericalCowApplicative name age weight = result
    where
        maybeName = noEmpty name
        maybeAge = noNegative age
        maybeWeight = noNegative weight
        maybeCow = cowL maybeName maybeAge maybeWeight 
        weightCheckedCow = weightCheck <$> maybeCow
        -- but here there is a problem because the type is: Maybe (Maybe Cow), so flatten it
        result = join weightCheckedCow 
                                                
