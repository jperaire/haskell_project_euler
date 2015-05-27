sum_square_difference :: Integral a => a -> a
sum_square_difference a = square_of_sum - sum_of_squares
    where
        square_of_sum = (^2) . sum $ [1..a]
        sum_of_squares = sum . map (^2) $ [1..a]