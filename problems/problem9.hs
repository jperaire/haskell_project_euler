specialPythagoreanTriplet :: Integral a => a
specialPythagoreanTriplet = maximum [x*y*z | x <- [1..1000], y <- [1..1000-x], z <- [1000-x-y], x^2 + y^2 == z^2]

--[x * y * z | x<-[1..1000],y<-[10-x..1000], z<-[1000-x-y], x^2+y^2==z^2]

