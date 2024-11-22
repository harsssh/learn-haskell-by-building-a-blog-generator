newtype Html = Html String

newtype Structure = Structure String

type Title = String

getStructureString :: Structure -> String
getStructureString (Structure str) = str

append_ :: Structure -> Structure -> Structure
append_ (Structure s) (Structure t) = Structure (s <> t)

render :: Html -> String
render (Html s) = s

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

html_ :: Title -> Structure -> Html
html_ title content =
  Html
    ( el
        "html"
        ( el "head" (el "title" title)
            <> el "body" (getStructureString content)
        )
    )

p_ :: String -> Structure
p_ = Structure . el "p"

h1_ :: String -> Structure
h1_ = Structure . el "h1"

myhtml :: Html
myhtml =
  html_
    "My page title"
    (append_ (h1_ "Heading") (p_ "Paragram #1"))

main :: IO ()
main = putStrLn (render myhtml)
