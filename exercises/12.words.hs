newtype Word' =
    Word' String
    deriving (Eq, Show)
vowels = "aeiou"

getVowelConsonantCounts :: String -> (Int, Int)
getVowelConsonantCounts s = (vowelCount, consonantCount) where
    vowelCount = length (filter (\x -> elem x vowels) s)
    consonantCount = length s - vowelCount

mkWord :: String -> Maybe Word'
mkWord s = result where
    vowelCount = length (filter (\x -> elem x vowels) s)
    consonantCount = length s - vowelCount
    result = 
        case compare vowelCount consonantCount of
            GT -> Nothing
            otherwise -> Just (Word' s)

mkWord3 :: String -> Maybe Word'
mkWord3 s = result where
        vowelCount = length (filter (\x -> elem x vowels) s)
        consonantCount = length s - vowelCount
        vowelsComparedToConsonants = compare vowelCount consonantCount
        result
            | vowelsComparedToConsonants == GT =  Nothing
            | otherwise = Just (Word' s)

mkWord2 :: String -> Maybe Word'
mkWord2 s = 
    case compare vowelCount consonantCount of
        GT -> Nothing
        otherwise -> Just (Word' s) 
    where
        (vowelCount, consonantCount) = getVowelConsonantCounts s

mkWordG :: String -> Maybe Word'
mkWordG s 
    | vowelsComparedToConsonants == GT =  Nothing
    | otherwise = Just (Word' s) where
        vowelCount = length (filter (\x -> elem x vowels) s)
        consonantCount = length s - vowelCount
        vowelsComparedToConsonants = compare vowelCount consonantCount

mkWordG2 :: String -> Maybe Word'
mkWordG2 s 
      | (compare vowelCount consonantCount) == GT =  Nothing
      | otherwise = Just (Word' s) where
            vowelCount = length (filter (\x -> elem x vowels) s)
            consonantCount = length s - vowelCount


mkWordC2 :: String -> Maybe Word'
mkWordC2 s = 
    let 
        vowelCount = length (filter (\x -> elem x vowels) s)
        consonantCount = length s - vowelCount
    in
        case compare vowelCount consonantCount of
            GT -> Nothing
            otherwise -> Just (Word' s)

