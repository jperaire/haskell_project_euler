import Data.Ratio (denominator, (%))

extra_factor_needed :: Integral a => a -> a -> a
extra_factor_needed a b = denominator . (%) a $ b

smallest_multiple :: Integral a => a -> a
smallest_multiple a = foldr f 1 [1..a]
    where f new acc = acc * extra_factor_needed acc new