import Data.List
import Data.Char
import Data.Function

{-
2) Define a function applyWhile that will apply a function repeatedly to a starting value until another function returns false about the result, returning the result that violated the while function.
So
     applyWhile (<2) (*2) 1
              should return 2 because the initial answer of 1*2 is no longer < 2,
while
      applyWhile (<100) (*2) 1
            should return 128, because 128 is the first power of 2 not less than 100
-}
applyWhile :: (Eq a, Ord a, Num a) => (a -> Bool) -> (a -> a) -> a -> a
applyWhile c f x
         | not $ c $ f x = f x
         | otherwise = applyWhile c f $ f x
