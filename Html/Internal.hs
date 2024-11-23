module Html.Internal where

import GHC.Natural (Natural)

newtype Html = Html String

newtype Structure = Structure String

instance Semigroup Structure where
  (<>) (Structure s) (Structure t) = Structure (s <> t)

instance Monoid Structure where
  mempty = Structure ""

type Title = String

getStructureString :: Structure -> String
getStructureString (Structure str) = str
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
        ( el "head" (el "title" (escape title))
            <> el "body" (getStructureString content)
        )
    )

p_ :: String -> Structure
p_ = Structure . el "p" . escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

h_ :: Natural -> String -> Structure
h_ n =
  let tag = "h" <> show n
   in Structure . el tag . escape

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . concatMap (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . concatMap (el "li" . getStructureString)

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

escape :: String -> String
escape =
  let escapeChar c =
        case c of
          '<' -> "&lt;"
          '>' -> "&gt;"
          '&' -> "&amp;"
          '"' -> "&quot;"
          '\'' -> "&#39;"
          _ -> [c]
   in concatMap escapeChar

concatStructure :: [Structure] -> Structure
concatStructure = mconcat

