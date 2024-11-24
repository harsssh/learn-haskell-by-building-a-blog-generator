module HsBlog (process, convertSingle) where

import HsBlog.Convert (convert)
import qualified HsBlog.Markup as Markup
import qualified HsBlog.Html as Html
import System.IO (Handle, hGetContents, hPutStrLn)

process :: Html.Title -> String -> String
process title = Html.render . convert title . Markup.parse

convertSingle :: Html.Title -> Handle -> Handle -> IO()
convertSingle title input output = do
  content <- hGetContents input
  hPutStrLn output $ process title content
