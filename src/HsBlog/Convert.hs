module HsBlog.Convert where

import HsBlog.Html qualified as Html
import HsBlog.Markup qualified as Markup

convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
  case structure of
    Markup.Heading n txt -> Html.h_ n $ Html.txt_ txt
    Markup.Paragraph p -> Html.p_ $ Html.txt_ p
    Markup.UnorderedList list -> Html.ul_ $ map (Html.p_ . Html.txt_) list
    Markup.OrderedList list -> Html.ol_ $ map (Html.p_ . Html.txt_) list
    Markup.CodeBlock list -> Html.code_ $ unlines list

convert :: Html.Title -> Markup.Document -> Html.Html
convert title = Html.html_ title . foldMap convertStructure
