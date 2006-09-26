module Common (WikiInfo(WikiInfo), basenames, datadir, basename) where

import FilePath

data WikiInfo = WikiInfo { basenames :: [String] }

datadir = "./data/"

basename = (\(_,m,_)->m).splitFilePath
