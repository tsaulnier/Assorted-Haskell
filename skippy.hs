-- accept an arbitrary list and prints odd elements

skippy :: [a] -> [a]
skippy [] = []
skippy [x] = [x]
skippy (x:_:xs) = x : skippy xs
