is_prime :: Integral a => a -> Bool
is_prime a = not . any (0 ==) . map (a `mod`) $ takeWhile (\x -> x^2 <= a) (2:[3,5..])

nth_prime :: Integral a => a -> a
nth_prime n = head . drop (fromIntegral n) . filter is_prime $ 2:[3,5..]