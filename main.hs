{-# LANGUAGE LambdaCase #-}

import Control.Monad qualified
import Convert (convert)
import Html
import Markup qualified
import System.Directory (doesFileExist)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      content <- getContents
      putStrLn $ process "Empty title" content
    [infile, outfile] -> do
      exists <- doesFileExist outfile
      let writeResult = readFile infile >>= writeFile outfile
      if exists
        then whenIO confirm writeResult
        else writeResult
    _ ->
      putStrLn "Usage: runghc Main.hs [-- <input-file> <output-file>]"

process :: Html.Title -> String -> String
process title = render . convert title . Markup.parse

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action = do
  result <- cond
  Control.Monad.when result action

confirm :: IO Bool
confirm = do
  putStrLn "Are you sure? (y/n)"
  answer <- getLine
  case answer of
    "y" -> pure True
    "n" -> pure False
    _ -> do
      putStrLn "Invalid response. use y or n"
      confirm
