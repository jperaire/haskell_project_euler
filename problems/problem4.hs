to_list :: Integral a => a -> [a]
to_list x 
    | abs x < 10 = [x]
    | otherwise = (rem x 10):(to_list (quot x 10))

is_palindrome :: Integral a => a -> Bool
is_palindrome x = reverse x_list == x_list
    where x_list = to_list x

largest_palindrome :: Integral a => a
largest_palindrome = maximum . filter is_palindrome $ [x * y | x <- tdms, y <- [x..999]]
    where tdms = [100..999]