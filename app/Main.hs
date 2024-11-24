module Main where

import Control.Exception (bracket)
import HsBlog qualified
import HsBlog.Directory qualified
import OptParse
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.IO

main :: IO ()
main = do
  options <- parse
  case options of
    ConvertDir input output ->
      HsBlog.Directory.convertDirectory input output
    ConvertSingle input output -> do
      let (title, withInputHandle) =
            case input of
              Stdin -> ("", ($ stdin))
              InputFile file ->
                (file, withFile file ReadMode)

      let withOutputHandle =
            case output of
              Stdout -> ($ stdout)
              OutputFile file ->
                bracket
                  ( do
                      exists <- doesFileExist file
                      shouldOpenFile <-
                        if exists
                          then confirm
                          else pure True
                      if shouldOpenFile
                        then openFile file WriteMode
                        else exitFailure
                  )
                  hClose

      withInputHandle $ \inputHandle ->
        withOutputHandle $ \outputHandle ->
          HsBlog.convertSingle title inputHandle outputHandle

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
