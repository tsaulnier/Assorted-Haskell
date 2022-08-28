-- prints the contents of a file in alphabetical order

import System.Environment
import System.Directory
import System.IO
import Data.List

dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("alphaLines", alphaLines) ]

main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

alphaLines :: [String] -> IO ()
alphaLines [fileName] = do
  contents <- readFile fileName
  let sortedLines = sort $ lines contents
  putStr $ unlines sortedLines
