import Data.List
import Data.Char
import Data.Function

{-
3) Write a function wordFreq that will compute the number of times each word appeared in a text string. Two words should be considered the same if they have the same letters, regardless of their capitalization. The result should be returned in an appropriate Map.
-}
wordFreq :: String -> [(String, Int)]
wordFreq s = reverse $ sortBy (compare `on` snd) $ map (\w -> (head w, length w)) $ group $ reverse $ sort $ words $ map toLower s
