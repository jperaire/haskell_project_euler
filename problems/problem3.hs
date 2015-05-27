is_prime :: Integral a => a -> Bool
is_prime a = not . any (0 ==) . map (a `mod`) $ takeWhile (\x -> x^2 <= a) (2:[3,5..])

smallest_divisor :: Integral a => a -> a
smallest_divisor x
    | is_prime x = x
    | otherwise = head . filter ((0 ==) . (x `mod`)) $ 2:[3,5..]

prime_factors :: Integral a => a -> [a]
prime_factors 1 = [1]
prime_factors n = d : prime_factors (n `div` d)
    where d = smallest_divisor n

largest_prime_factor :: Integral a => a -> a
largest_prime_factor = maximum . prime_factors