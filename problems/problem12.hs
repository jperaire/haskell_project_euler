import Data.List (group)

--numDivisors :: Integral a => a -> a
--numDivisors a = fromIntegral . length . filter (\x -> a `mod` x == 0) $ takeWhile (\x -> 2*x < a) [1..]

triangleNumbers :: Integral a => [a]
triangleNumbers = scanl (+) 1 $ [2..]

wantedTriangleNumber :: Integral a => a -> a
wantedTriangleNumber a = head . filter ((>a) . numDivisors) $ triangleNumbers


-- Based on prime factorization. Input should be list of how many times each prime factor appears. I.e. 2^4 * 3^2 should be
-- [4,2]

fac :: Integral a => a -> a
fac a = product [1..a]

isPrime :: Integral a => a -> Bool
isPrime a = not . any (0 ==) . map (a `mod`) $ takeWhile (\x -> x^2 <= a) (2:[3,5..])

primeFactorization :: Integral a => a -> [a]
primeFactorization a
    | isPrime a = [a]
    | otherwise = minDivisor : (primeFactorization . div a $ minDivisor)
        where minDivisor = head . filter (\x -> a `mod` x == 0) $ (2:[3,5..])

numDivisors :: Integral a => a -> a
numDivisors a = product . map (+1) $ xs
    where xs = map (fromIntegral . length) . group $ primeFactorization a 