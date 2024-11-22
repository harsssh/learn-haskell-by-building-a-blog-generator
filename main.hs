wrapHtml content = "<html><body>" <> content <> "</body></html>"

myhtml = wrapHtml "Hello, world!"

main = putStrLn myhtml
