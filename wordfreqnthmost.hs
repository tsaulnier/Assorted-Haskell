import Data.List
import Data.Char
import Data.Function

{-
4) Building upon 3), return a list of the most common n words (where n is a parameter to the function) each as a string.
*** a little vague but interpreting as "return the n most common words in a given string"***
-}
wordFreqNthMost :: String -> Int -> [String]
wordFreqNthMost s n = take n $ map fst $ reverse $ sortBy (compare `on` snd) $ map (\w -> (head w, length w)) $ group $ reverse $ sort $ words $ map toLower s
