-- gives sum of alternating elements in a list, starting with first element

altSum :: Num a => [a] -> a
altSum [] = 0
altSum [x] = x
altSum x = sum $ map fst $ filter (even.snd) $ zip x [0..]
