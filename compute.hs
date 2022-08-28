{-
3) Write a function compute that takes a tuple with an operator (given as a single character) and two numbers and returns the value from computing the result of that operation on the given numbers.
For example, compute ('+',3,4) should return 7.
-}

compute :: Num a => (Char, a, a) -> a
compute ('+',f1,f2) = f1 + f2
compute ('-',f1,f2) = f1 - f2
compute ('*',f1,f2) = f1 * f2
