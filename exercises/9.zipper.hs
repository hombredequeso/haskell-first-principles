zipper :: [a] -> [b] -> [(a, b)]
-- zipper xs1 xs2 = undefined
zipper [] [] = []
zipper (x:_) [] = []
zipper [] (x:_) = []
zipper (x:xs) (y:ys) = 
            f x y:(zipperWith f xs ys)
                where f = (,)

zipperWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipperWith f [] [] = []
zipperWith f (x:_) [] = []
zipperWith f [] (x:_) = []
zipperWith f (x:xs) (y:ys) = f x y:(zipperWith f xs ys)

