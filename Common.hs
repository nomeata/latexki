module Common (
	FileProcessor,
	DepCalculator,

	WikiInfo(WikiInfo),
	sitemap,
	wikiConfig,
	mainTitle,
	basenames,
	outputs,

	datadir,

	triple1,
	triple2,
	triple3,

	basename,
	splitFilePath,
	filename,
) where

import qualified FilePath as FP
import Maybe

-- Data type

type FileProcessor = FilePath -> WikiInfo -> IO ()
type DepCalculator = FilePath -> WikiInfo -> IO [FilePath]

data WikiInfo = WikiInfo { sitemap :: [(String, String, [String]) ] , wikiConfig :: [(String,String)] }
mainTitle = (fromMaybe "A Wiki").(lookup "title").wikiConfig

basenames wi = map triple1 (sitemap wi)

outputs wi = concatMap ( \(b,_,exts) -> map ((b++".")++) exts) (sitemap wi)

datadir = "./data/"

basename = triple2.splitFilePath
filename = snd.FP.splitFileName 
splitFilePath = FP.splitFilePath

triple1 (x,_,_) = x
triple2 (_,x,_) = x
triple3 (_,_,x) = x
