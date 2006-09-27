module HtmlStyle (htmlPage, tagP, tag, tagL, aHref) where

htmlPage wi title body = 
  "<html><head><title>"++title++
  "</title></head><body>"++body++"</body></html>"

tagP name params body = "<"++name++concatMap (\(p,v)-> " "++p++"=\""++v++"\"") params ++">"++body++"</"++name++">"
tag name body= tagP name [] body
tagL name body= ["<"++name++">"]++body++["</"++name++">"]

aHref href body = tagP "a" [("href",href)] body
