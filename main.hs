{-# LANGUAGE LambdaCase #-}

import Control.Monad qualified
import Convert (convert)
import Html
import Markup qualified
import System.Directory (doesFileExist)
import System.Environment (getArgs)

main :: IO ()
main =
  getArgs >>= \case
    [] ->
      getContents >>= \content ->
        putStrLn $ process "Empty title" content
    [infile, outfile] ->
      doesFileExist outfile >>= \exists ->
        let writeResult = readFile infile >>= \content -> writeFile outfile $ process infile content
         in if exists
              then whenIO confirm writeResult
              else writeResult
    _ ->
      putStrLn "Usage: runghc Main.hs [-- <input-file> <output-file>]"

process :: Html.Title -> String -> String
process title = render . convert title . Markup.parse

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action =
  cond >>= \result -> Control.Monad.when result action

confirm :: IO Bool
confirm =
  putStrLn "Are you sure? (y/n)"
    *> getLine
    >>= \case
      "y" -> pure True
      "n" -> pure False
      _ ->
        putStrLn "Invalid response. use y or n"
          *> confirm
