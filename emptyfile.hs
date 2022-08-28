-- creates files in directory and skips files that already exist

emptyFile [] = return ()
emptyFile (x:xs) = do
          let fileName = x
          fileExist <- doesFileExist fileName
          if not fileExist
          then openFile fileName
          else emptyFile xs
          emptyFile xs
