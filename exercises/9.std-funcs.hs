myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem e [] = False
myElem e (x:xs) = (e == x) || myElem e xs


myElemA :: Eq a => a -> [a] -> Bool
myElemA e xs = any (\x -> x == e) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f [] = []
squishMap f (x:xs) = (f x) ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain x = squishMap id x

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ (x:[]) = x
myMaximumBy f (x:y:tail) = myMaximumBy f (max:tail) where
                                max =
                                    case comparisonResult of
                                      LT -> y
                                      EQ  -> x
                                      GT -> x
                                    where comparisonResult = f x y

myMaximumByM :: (a -> a -> Ordering) -> [a] -> Maybe a
myMaximumByM _ [] = Nothing
myMaximumByM f (x:xs) = go f xs x where
    go :: (a -> a -> Ordering) -> [a] -> a -> Maybe a
    go f [] currentMax = Just currentMax
    go f (x:xs) currentMax = go f xs newMax where
                                newMax =
                                    case comparisonResult of
                                      LT -> currentMax 
                                      EQ  -> currentMax
                                      GT -> x
                                    where comparisonResult = f x currentMax


myMaximumByN :: (a -> a -> Ordering) -> [a] -> a
myMaximumByN f (x:xs) = getByOrdering f GT x xs

myMinimumByN :: (a -> a -> Ordering) -> [a] -> a
myMinimumByN f (x:xs) = getByOrdering f LT x xs

getByOrdering  :: (a -> a -> Ordering) -> Ordering -> a -> [a] -> a
getByOrdering _ _ m [] = m
getByOrdering orderer order m (x:xs) = getByOrdering orderer order newMax xs where
                        newMax =
                            case comparisonResult of
                              True -> m
                              False -> x
                            where comparisonResult = (orderer m x) == order


