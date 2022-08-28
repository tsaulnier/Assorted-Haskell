import Data.List
import Data.Char
import Data.Function

{-
1) Provide a  recursive definition for a countValue function that will return the number of times value appears in a list.
So countValue 2 [1,2,7,2,3,8,2,4] should return 3
-}
countValue :: (Num a, Eq a) => a -> [a] -> Int
countValue _ [] = 0
countValue x (y:ys)
         | x == y = 1 + countValue x ys
         | otherwise = countValue x ys
