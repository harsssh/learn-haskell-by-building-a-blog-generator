import Html

main :: IO ()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html_
    "My page title"
    (append_ (h1_ "Heading") (p_ "Paragram #1"))
