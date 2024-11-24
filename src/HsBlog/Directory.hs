module HsBlog.Directory (buildIndex, convertDirectory) where

import Control.Exception (Exception (displayException), try, SomeException)
import Control.Monad (void, when)
import Data.Functor (($>))
import Data.List (partition)
import HsBlog.Convert (convert, convertStructure)
import HsBlog.Html qualified as Html
import HsBlog.Markup qualified as Markup
import System.Directory
  ( copyFile,
    createDirectory,
    doesDirectoryExist,
    listDirectory,
    removeDirectoryRecursive,
  )
import System.Exit (exitFailure)
import System.FilePath
  ( takeBaseName,
    takeExtension,
    takeFileName,
    (<.>),
    (</>),
  )
import System.IO (hPutStrLn, stderr)

convertDirectory :: FilePath -> FilePath -> IO ()
convertDirectory inputDir outputDir = do
  DirContents filesToProcess filesToCopy <- getDirFilesAndContent inputDir
  createOutputDirectoryOrExit outputDir
  let outputHtmls = txtsToRenderedHtml filesToProcess
  copyFiles outputDir filesToCopy
  writeFiles outputDir outputHtmls
  putStrLn "Done."

buildIndex :: [(FilePath, Markup.Document)] -> Html.Html
buildIndex files =
  Html.html_
    "Blog"
    $ Html.h_ 1 (Html.link_ "index.html" $ Html.txt_ "Blog")
      <> Html.h_ 2 (Html.txt_ "Posts")
      <> mconcat previews
  where
    previews =
      map
        ( \(file, doc) ->
            case doc of
              Markup.Heading 1 heading : article ->
                Html.h_ 3 (Html.link_ file $ Html.txt_ heading)
                  <> foldMap convertStructure (take 3 article)
                  <> Html.p_ (Html.link_ file $ Html.txt_ "...")
              _ ->
                Html.h_ 3 $ Html.link_ file $ Html.txt_ file
        )
        files

data DirContents
  = DirContents
  { dcFilesToProcess :: [(FilePath, String)],
    dcFilesToCopy :: [FilePath]
  }

getDirFilesAndContent :: FilePath -> IO DirContents
getDirFilesAndContent inputDir = do
  files <- map (inputDir </>) <$> listDirectory inputDir
  let (txtFiles, otherFiles) = partition ((== ".txt") . takeExtension) files
  txtFilesAndContent <-
    applyIoOnList readFile txtFiles
      >>= filterAndReportFailures
  pure $ DirContents {dcFilesToProcess = txtFilesAndContent, dcFilesToCopy = otherFiles}

applyIoOnList :: (a -> IO b) -> [a] -> IO [(a, Either String b)]
applyIoOnList f = traverse $ \input ->
  try @SomeException (f input)
    >>= \r -> pure (input, either (Left . displayException) Right r)

filterAndReportFailures :: [(a, Either String b)] -> IO [(a, b)]
filterAndReportFailures =
  foldMap $ \case
    (_, Left err) -> hPutStrLn stderr err $> []
    (x, Right suc) -> pure [(x, suc)]

createOutputDirectoryOrExit :: FilePath -> IO ()
createOutputDirectoryOrExit outputDir =
  whenIO
    (not <$> createOutputDirectory outputDir)
    (hPutStrLn stderr "Cancelled." *> exitFailure)

createOutputDirectory :: FilePath -> IO Bool
createOutputDirectory dir = do
  dirExists <- doesDirectoryExist dir
  create <-
    if dirExists
      then do
        override <- confirm "Output directory exists. Override?"
        when override (removeDirectoryRecursive dir)
        pure override
      else
        pure True
  when create (createDirectory dir)
  pure create

txtsToRenderedHtml :: [(FilePath, String)] -> [(FilePath, String)]
txtsToRenderedHtml txtFiles =
  let txtOutputFiles = map toOutputMarkupFile txtFiles
      index = ("index.html", buildIndex txtOutputFiles)
   in map (fmap Html.render) (index : map convertFile txtOutputFiles)

toOutputMarkupFile :: (FilePath, String) -> (FilePath, Markup.Document)
toOutputMarkupFile (file, content) =
  (takeBaseName file <.> "html", Markup.parse content)

convertFile :: (FilePath, Markup.Document) -> (FilePath, Html.Html)
convertFile (file, doc) = (file, convert file doc)

copyFiles :: FilePath -> [FilePath] -> IO ()
copyFiles outputDir files = do
  let copyFromTo file = copyFile file (outputDir </> takeFileName file)
  void $ applyIoOnList copyFromTo files >>= filterAndReportFailures

writeFiles :: FilePath -> [(FilePath, String)] -> IO ()
writeFiles outputDir files = do
  let writeFileContent (file, content) =
        writeFile (outputDir </> file) content
  void $ applyIoOnList writeFileContent files >>= filterAndReportFailures

-- Utilities
confirm :: String -> IO Bool
confirm question = do
  putStrLn (question <> " (y/n)")
  answer <- getLine
  case answer of
    "y" -> pure True
    "n" -> pure False
    _ -> do
      putStrLn "Invalid response. Use y or n."
      confirm question

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action = do
  result <- cond
  when result action
