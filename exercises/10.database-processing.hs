import Data.Time

data DatabaseItem = 
    DbString String
    | DbNumber Integer
    | DbDate UTCTime
    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ 
        DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
        , DbNumber 9001
        , DbString "Hello, world!"
        , DbNumber 70
        , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate xs = foldr foldFunc [] xs where
                    foldFunc (DbDate t) acc = t : acc
                    foldFunc _ acc = acc

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber xs = foldr foldFunc [] xs where
                    foldFunc (DbNumber i) acc = i : acc
                    foldFunc _ acc = acc

mostRecentUTCTime :: [UTCTime] -> UTCTime
mostRecentUTCTime [] = undefined
mostRecentUTCTime (x:xs) = foldr (\y c -> max y c) x xs

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = mostRecentUTCTime . filterDbDate

mySum :: Num a => [a] -> a
mySum = (foldr (+) 0)

sumDb :: [DatabaseItem] -> Integer
sumDb = mySum . filterDbNumber 

sumAndCount :: Num a =>  [a] -> (a, Integer)
sumAndCount xs = foldr (\x acc -> (fst acc + x, snd acc + 1)) (0,0) xs

avgDb :: [DatabaseItem] -> Double
avgDb xs =  (fromIntegral total) / (fromIntegral count) where
               dbNums = filterDbNumber xs
               (total, count) = sumAndCount dbNums
               
