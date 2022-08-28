{-
2) Define a function factors that returns a list of all the factors of the number given. For example, factors 12 returns [1,2,3,4,6,12].
-}

factors :: (Integral a, Eq a) => a -> [a]
factors x = [f | f <- [1..x],  x `mod` f == 0]
