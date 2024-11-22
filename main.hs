el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

html_ :: String -> String
html_ = el "html"

head_ :: String -> String
head_ = el "head"

title_ :: String -> String
title_ = el "title"

body_ :: String -> String
body_ = el "body"

makeHtml :: String -> String -> String
makeHtml title content = html_ (head_ (title_ title) <> body_ content)

myhtml :: String
myhtml = makeHtml "My page title" "My page content"

main :: IO ()
main = putStrLn myhtml
