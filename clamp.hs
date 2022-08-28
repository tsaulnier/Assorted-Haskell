-- the first argument to clamp defines the lower bound of results
-- and the second argument to clamp defines the upper bound to results.
-- If the third argument is within those bounds, that value is returned.
-- If the third value is below the range, the lower bound is returned.
-- And if the third value is above the range, the upper bound is returned. So
-- clamp 1 10 3 returns 3 
-- clamp 1 10 0 returns 1 
-- clamp 1 10 20 returns 10

clamp :: (Ord a) => a -> a -> a -> a
clamp lower upper = min upper . max lower
