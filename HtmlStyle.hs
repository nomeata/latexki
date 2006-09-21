module HtmlStyle (htmlPage) where

htmlPage wi title body = 
  "<html><head><title>"++title++
  "</title></head><body>"++body++"</body></html>"
