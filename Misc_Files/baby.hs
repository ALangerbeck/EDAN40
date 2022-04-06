doubleMe x = x + x

doubleUs x y = x*2 + y*2

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)
