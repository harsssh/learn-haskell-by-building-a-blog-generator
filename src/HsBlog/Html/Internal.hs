module HsBlog.Html.Internal where

import GHC.Natural (Natural)

-- Types

newtype Html = Html String

newtype Content = Content String

newtype Structure = Structure String

type Title = String

-- EDSL

html_ :: Title -> Structure -> Html
html_ title content =
  Html
    ( el
        "html"
        ( el "head" (el "title" (escape title))
            <> el "body" (getStructureString content)
        )
    )

-- Render

render :: Html -> String
render (Html s) = s

-- Structure
instance Semigroup Structure where
  (<>) (Structure s) (Structure t) = Structure (s <> t)

instance Monoid Structure where
  mempty = Structure ""

p_ :: Content -> Structure
p_ = Structure . el "p" . getContentString

h_ :: Natural -> Content -> Structure
h_ n =
  let tag = "h" <> show n
   in Structure . el tag . getContentString

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . concatMap (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . concatMap (el "li" . getStructureString)

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

-- Content
instance Semigroup Content where
  (<>) (Content s) (Content t) = Content $ s <> t

instance Monoid Content where
  mempty = Content ""

txt_ :: String -> Content
txt_ = Content . escape

link_ :: FilePath -> Content -> Content
link_ path =
  Content . elAttr "a" (makeAttributes [("href", escape path)]) . getContentString

img_ :: FilePath -> Content
img_ path = Content $ elAttrSelf "img" $ makeAttributes [("src", escape path)]

b_ :: Content -> Content
b_ = Content . el "b" . getContentString

i_ :: Content -> Content
i_ = Content . el "i" . getContentString

-- Utilities
getStructureString :: Structure -> String
getStructureString (Structure str) = str

getContentString :: Content -> String
getContentString (Content str) = str

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

elAttr :: String -> String -> String -> String
elAttr tag attrs content =
  "<" <> tag <> " " <> attrs <> ">" <> content <> "</" <> tag <> ">"

elAttrSelf :: String -> String -> String
elAttrSelf tag attrs =
  "<" <> tag <> " " <> attrs <> "/>"

makeAttributes :: [(String, String)] -> String
makeAttributes =
  unwords . map (\(key, value) -> key <> surround value "\"")

surround :: String -> String -> String
surround target s = s <> target <> s

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
