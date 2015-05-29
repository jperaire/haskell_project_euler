primes :: Integral a => [a] -> [a]
primes (x:xs) = x:(primes . filter ((/=0) . (`mod` x)) $ xs)

primesBelow :: Integral a => a -> [a] -> [a]
primesBelow lim y@(x:xs) 
    | (x-1)^2 <= lim = x:(primesBelow lim . filter ((/=0) . (`mod` x)) $ xs)
    | otherwise = takeWhile (<=lim) y

sumOfPrimesBelow :: Integral a => a -> a
sumOfPrimesBelow a = sum . primesBelow a $ [2..]