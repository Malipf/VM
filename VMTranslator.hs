module VMTranslator where

import qualified Parser (parseFile)
import qualified CodeWriter
import System.Environment (getArgs)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>), takeBaseName, replaceExtension) 

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      isDir <- doesDirectoryExist path
      vmFiles <- if isDir then getVMFiles path else return [path]
      allCommands <- mapM Parser.parseFile vmFiles

      let commandsWithFile = zip vmFiles allCommands
      asmLines <- concatMap (uncurry CodeWriter.writeFileCommands) commandsWithFile

      let baseName = if isDir then path else takeBaseName path
      outFile      <- if isDir then path </> (baseName ++ ".asm")
                              else replaceExtension path ".asm"

      writeFile outFile (unlines asmLines)
      putStr "Successfully written to: "
      print outFile
    _      -> putStrLn "Usage: VMTranslator.hs <inputFile.vm> or <inputDirectory>"
  

getVMFiles :: FilePath -> IO [FilePath]
getVMFiles dir = do
  files <- listDirectory dir
  return [dir </> f | f <- files, takeExtension f == ".vm"]
  
