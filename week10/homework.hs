isPrime :: Int -> Bool
isPrime n
    | n <= 1 = False
    | otherwise = isPrime' 2 n
        where
            isPrime' current n
                | current == n = True
                | mod n current == 0 = False
                | otherwise = isPrime' (current + 1) n


truncatablePrime :: Int -> Bool
truncatablePrime x
    | x == 0 = True
    | otherwise = isPrime x && truncatablePrime (div x 10)

containsDigit :: Int -> Int -> Bool -> Bool
containsDigit number digit answer
    | number == 0 = answer
    | otherwise = answer || (digit == (mod number 10)) || containsDigit (div number 10) digit False

containsDigits :: Int -> Int -> Bool
containsDigits x y
    | y < 10 = containsDigit x y False
    | otherwise = (containsDigit x (mod y 10) False) && containsDigits x (div y 10)

productOfDigits :: Int -> Int
productOfDigits x
    | x < 10 = x
    | otherwise = mod x 10 * productOfDigits (div x 10)


dn :: Int -> Int
dn number
    | number <= 10  = number
    | otherwise = dn' number 0 1
        where
            dn' number answer current
                | current == number = answer
                | mod number current == 0 = answer + current + dn' number answer (current + 1)
                | otherwise = dn' number answer (current + 1)

interestingNumber :: Int -> Bool
interestingNumber number = number == dn(dn(number))

quadrant :: Double -> Double -> Int
quadrant x y
    | x == 0 && y == 0 = 0
    | x >= 0 && y >= 0 = 1
    | x < 0 && y > 0 = 2
    | x <= 0 && y <= 0 = 3
    | x > 0 && y < 0 = 4
