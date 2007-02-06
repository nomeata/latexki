module PDF (splitPDF, guessSplitPDF, formatPDFInfo, getPDFInfo,  PDFData(..), PDFIndex(..)) where

import Data.Maybe
import Control.Monad
import Directory
import System.Process
import System.IO
import Data.List

import WikiData
import Common

data PDFIndex = PDFIndex { pdfIndexTitle :: String, pdfIndexPage :: Int, pdfIndexSub :: [PDFIndex] } deriving (Show)
data PDFData = PDFData { numberOfPages :: Int, pdfIndex :: [PDFIndex] } deriving (Show)

data Fields = NumberOfPages Int | Title String | Level Int | Page Int deriving (Show)

parseField "NumberOfPages" val = Just (NumberOfPages (read val))
parseField "BookmarkPageNumber" val = Just (Page (read val))
parseField "BookmarkTitle" val = Just (Title val)
parseField "BookmarkLevel" val = Just (Level (read val))
parseField _		   _   = Nothing

isIndexField (Title _) = True
isIndexField (Level _) = True
isIndexField (Page _)  = True
isIndexField _         = False
isTitle (Title _) = True
isTitle _         = False

getField line = do
  	let (field',_:_:val) = span (/=':') line
	parseField field' val

putInShape :: ([Fields],[Fields]) -> PDFData
putInShape (index_raw, meta) = PDFData { numberOfPages = number, pdfIndex = index }
  where	number = nOfPages meta
        index  = unfoldElems 1 (groupIndex index_raw)

groupIndex :: [Fields] -> [[Fields]]
groupIndex = groupBy (\_ f -> not $ isTitle f)

nOfPages (NumberOfPages n:_) = n
nOfPages (_:x)               = nOfPages x
level (Level l:_) = l
level (_:x)       = level x
title (Title l:_) = l
title (_:x)       = title x
page  (Page l:_)  = l
page  (_:x)       = page x

unfoldElem :: Int -> [[Fields]] -> PDFIndex
unfoldElem l (this:sub) = PDFIndex { pdfIndexTitle = title this, pdfIndexPage = page this,
				       pdfIndexSub = unfoldElems (l+1) sub }

unfoldElems :: Int -> [[Fields]] -> [PDFIndex]
unfoldElems l = map (unfoldElem l) . groupBy (\_ n -> (level n > l))

parseDumpData = putInShape .  partition isIndexField . catMaybes . map getField . lines

getPDFInfo file = do
	let options = [file,"dump_data"]
	(inp, out, err, pid) <- runInteractiveProcess "pdftk" options Nothing Nothing
	hClose inp
	info <- hGetContents out
	return $ parseDumpData info

formatPDFInfo file info = [
	Header 3 "PDF-Index",
	Paragraph $ [Text $ "This PDF-File has "++show(numberOfPages info)++" pages"],
	ItemList $ (firstElem:) $ map formatPDFIndex $ zip [2..] $ pdfIndex info
	]
 where	firstElem = [LinkElem $ PlainLink (subfile 1) "(First Pages)" ]
 	formatPDFIndex (n, (PDFIndex title page sub)) = [Text title, Text " ",
		LinkElem $ PlainLink (subfile n)  $ "(Page "++ show page++")"
		]
        subfile n = chapterFile file n

extractPDFPages infile outfile (from, to) = do
	let range = (show from) ++ "-" ++ fromMaybe "end" (fmap show to)
	let options = [infile,"cat",range,"output",outfile]
	pid <- runProcess "pdftk" options Nothing Nothing Nothing Nothing Nothing
	waitForProcess pid
	return ()

ranges [] = []
ranges (x:xs) | pdfIndexPage x > 1 = (1,Just (pdfIndexPage x - 1)) : ranges'(x:xs)
              | otherwise          =                                 ranges'(x:xs)
ranges' [] = []
ranges' [last] =   [(pdfIndexPage last,Nothing)]
ranges' (x:y:xs) = (pdfIndexPage x,Just (pdfIndexPage y -1 )) : ranges' (y:xs)

chapterFile file n = file ++ "." ++ (show n) ++ ".pdf"

splitPDF file info = do
	let chapters = zip [1..] (ranges (pdfIndex info))
	date1 <- getModificationTime file
	flip mapM chapters $ \(n,r) -> do
		let outfile = chapterFile file n
		doit <- newer date1 outfile
		when doit $ extractPDFPages file outfile r
		return outfile

-- We don’t want to parse the PDF if no change is expected, so just look for the files
guessSplitPDF file = do
	let dir = dirname file
	candits <- directoryFiles dir
	return $ filter (file `isPrefixOf`) candits

newer date file = do
	ex <- doesFileExist file
	if ex then do
		date2 <- getModificationTime file
		if date > date2 then
			return True
		 else
			return False
	 else return True


