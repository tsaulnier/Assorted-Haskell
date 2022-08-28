-- functions for displaying the paths of files in a directory

import System.Environment
import System.Directory
import System.IO
import Data.List
import System.FilePath.Posix

main = do
    args <- getArgs
    let path = if null args then "." else head args
    displayLogic "" 0 path

displayLogic :: FilePath -> Int -> FilePath -> IO ()
displayLogic path indent name = do
 let fPath = if null path then name else path </> name
 existDir <- doesDirectoryExist fPath
 let spaces = replicate indent " "
 putStr $ concat spaces
 putStr name
 if existDir then displayDir indent fPath else displayFile fPath

displayDir :: Int -> FilePath -> IO ()
displayDir indent dir = do
 putStrLn ""
 d <- listDirectory dir
 mapM_ (displayLogic dir (indent+4)) d

displayFile :: FilePath -> IO ()
displayFile file = do
  size <- getFileSize file
  putStr " Size: "
  putStr $ show size
  putStr " Executable: "
  p <- getPermissions file
  let e = executable p
  putStr $ show e
  putStr " Last Modified:"
  let modTime = getModificationTime file
  m <- modTime
  putStrLn $ show m
