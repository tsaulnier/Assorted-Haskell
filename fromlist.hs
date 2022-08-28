-- creates a set from a list of elements (without using fromList library functions)

import qualified Data.Set as Set

fromList' :: Ord a => [a] -> Set.Set a
fromList' [] = Set.empty
fromList' [x] = Set.singleton x
fromList' (x:xs) = Set.union (Set.singleton x) $ fromList' xs
