{-# LANGUAGE TypeApplications, AllowAmbiguousTypes, ExplicitForAll, ScopedTypeVariables #-}


stops = "pbtdkg"
vowels = "aeiou"

nouns = ["cat", "dog", "person", "bat", "ball"]
verbs = ["ate", "hit", "sat", "consider"]

-- 1a
allPatterns = [(c0, v, c1) | v <- vowels, c0 <- stops, c1 <- stops ]

-- 1b
pPatterns = filter (\(v,_,_) -> v == 'p') allPatterns

-- 1c
sentences = triPattern nouns verbs
-- [(c0, v, c1) | v <- verbs, c0 <- nouns, c1 <- nouns ]

triPattern xs ys = [(x0, y, x1) | y <- ys, x0 <- xs, x1 <- xs ]

-- 2
-- Average word length probably
seekritFunc x =
    div (sum (map length (words x)))
        (length (words x))


avgWordLength x =
    (fromIntegral $ sum (map length (words x))) /
        (fromIntegral $ length (words x))

myOr :: [Bool] -> Bool
myOr = foldr (||) False 

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x acc -> f x || acc) False

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\y acc -> y == x || acc ) False

myReverse :: [a] -> [a]
myReverse = foldr (\x acc -> acc ++ [x]) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x acc -> f x : acc) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x acc -> if f x then x:acc else acc) []

squish :: [[a]] -> [a]
squish = foldr (\x acc -> x ++ acc) []

-- How monadic of you!!
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\ x acc -> f x ++ acc) []

squishAgain :: [[a]] -> [a]
squishAgain = undefined

myCompareBy :: Eq a => (a -> a -> Ordering) -> Ordering -> a -> [a] -> a
myCompareBy orderf ordering start xs = 
    foldr 
        (\x current -> if (orderf x current == ordering) then x else current) 
        start 
        xs

myMaximumBy :: Eq a => (a -> a -> Ordering) -> [a] -> a
myMaximumBy orderf (x: xs) = myCompareBy orderf GT x xs
-- myMaximumBy orderf (x:xs) = foldr (\m currentMax -> if (orderf m currentMax == GT) then m else currentMax) x xs

myMinimumBy :: Eq a => (a -> a -> Ordering) -> [a] -> a
myMinimumBy orderf (x: xs) = myCompareBy orderf LT x xs
-- myMinimumBy orderf (x:xs) = foldr (\m currentMax -> if (orderf m currentMax == LT) then m else currentMax) x xs

