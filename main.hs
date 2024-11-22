html_ content = "<html>" <> content <> "</html>"
head_ content = "<head>" <> content <> "</head>"
title_ content = "<title>" <> content <> "</title>"
body_ content = "<body>" <> content <> "</body>"

makeHtml title content = html_ (head_ (title_ title) <> body_ content)

myhtml = makeHtml "My page title" "My page content"

main = putStrLn myhtml
