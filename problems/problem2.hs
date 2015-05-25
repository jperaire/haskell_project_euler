fibonacci_numbers :: Integral a => [a]
fibonacci_numbers = 1:2:(fibonacci_numbers_recurse 1 2)

fibonacci_numbers_recurse :: Integral a => a -> a -> [a]
fibonacci_numbers_recurse a b = (a+b) : fibonacci_numbers_recurse b (a+b)

sum_even_fibonacci_numbers :: Integral a => a -> a
sum_even_fibonacci_numbers lim = sum . takeWhile (<lim) $ even_fibonacci_numbers
    where even_fibonacci_numbers = filter (\x -> x `mod` 2 == 0) fibonacci_numbers
    -- function to filter could be (0 ==) . (`mod` 2) but I thought using non-pointless was clearer here