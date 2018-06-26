import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = rev. cap

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = (,) <$> rev <*> cap


tupledM :: [Char] -> ([Char], [Char])
tupledM = do
            a <- cap
            b <- rev
            return (a,b)

tupledM2 :: [Char] -> ([Char], [Char])
tupledM2 = cap >>=     (\a -> rev  >>= (\b -> return (a, b)))
--    (r -> a) >>= (\a -> (r -> a) >>= (\b -> return (a, b)))

