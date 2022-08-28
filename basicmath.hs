{-
1) Define a function called basicMath that takes a pair of numbers and returns a tuple with the sum, difference, product and quotient.
For example basicMath 8 2 should return (10, 6, 16, 4).
-}

basicMath :: Fractional a => a -> a -> (a, a, a, a)
basicMath x y = (x + y, x - y, x * y, x / y)
