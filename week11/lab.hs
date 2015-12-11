firstIsX (x:_) y = x == y

length' :: [a] -> Int
length' [] = 0
length' (_ : xs) = 1 + length' xs

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (n : ns) = n + sum' ns

null' :: [a] -> Bool
null' [] = True
null' _ = False


member' :: Eq a => a -> [a] -> Bool
member' _ [] = False
member' element (x : xs) = element == x || member' element xs

last' :: [a] -> a
last' [] = error "Undefined last for []"
last' (x : xs) = if null xs then x else last' xs

indexOf :: Eq a => a -> [a] -> Int
indexOf element [] = -1
indexOf element list = indexOf' element list 0
                    where
                        indexOf' element [] _ = -1
                        indexOf' element (x : xs) current = if element == x
                                                            then current
                                                            else indexOf' element xs (current + 1)

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys


addLast :: a -> [a] -> [a]
addLast element (x : xs)
    | null xs = [x, element]
    |otherwise = x : addLast element xs


range' :: (Ord t, Enum t) => t -> t -> [t]
range' x y
    | x > y = []
    |otherwise = x : range' (succ x) y


enumerate :: [a] -> [(Int, a)]
enumerate l = zip' [0..(length l)] l


indexOf2 :: Eq a => a -> [a] -> Int
indexOf2 y l = go (enumerate l)
    where
        go [] = -1
        go (x : xs)
            | snd x == y = fst x
            | otherwise = go xs


addLast2 :: a -> [a] -> [a]
addLast2 x xs = xs ++ [x]
