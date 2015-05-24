multiples_of_3_and_5 :: Integral a => a -> a
multiples_of_3_and_5 lim = sum [x | x <- [1..lim-1], or [x `mod` 3 == 0, x `mod` 5 == 0]]
