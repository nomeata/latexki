{-# LANGUAGE PatternGuards #-}
module PDF (splitPDF, formatPDFInfo, getPDFInfo,  PDFData(..), PDFIndex(..)) where

import Data.Maybe
import Control.Monad
import System.Directory
import System.Process
import System.IO
import System.FilePath
import Control.Concurrent
import Data.List
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char
import qualified Data.String.UTF8 as UTF8
import Data.Word (Word8)
import Debug.Trace

import WikiData
import ReadDir
import Common

data Fields = NumberOfPages Int | Title String | Level Int | Page Int deriving (Show)

parseField "NumberOfPages"      val = Just (NumberOfPages (read val))
parseField "BookmarkPageNumber" val = Just (Page (read val))
parseField "BookmarkTitle"      val = Just (Title (unescape val))
  where unescape ('&':'#':s) | (codepoint,';':r) <- span isNumber s
                             = encode [chr (read codepoint)]
                               ++ unescape r
        unescape (c:cs)      = c:unescape cs
        unescape []          = []
        encode :: [Char] -> String
        encode = map (chr . fromIntegral :: Word8 -> Char). UTF8.toRep . UTF8.fromString
parseField "BookmarkLevel"      val = Just (Level (read val))
parseField _               _   = Nothing


isIndexField (Title _) = True
isIndexField (Level _) = True
isIndexField (Page _)  = True
isIndexField _         = False
isTitle (Title _) = True
isTitle _         = False

getField line
    | (field',_:_:val) <- span (/=':') line = parseField field' val
    | otherwise = Nothing

putInShape :: ([Fields],[Fields]) -> PDFData
putInShape (index_raw, meta) = PDFData { numberOfPages = number, pdfIndex = index }
  where number      = nOfPages meta
        real_index  = unfoldElems 1 (groupIndex index_raw)
        index | null real_index                      = []
              | first_chapter == 1 = real_index
              | first_chapter  > 1 = first_elem : real_index
        first_elem = PDFIndex {
                pdfIndexTitle = "(First Pages)",
                pdfIndexPage = 1,
                pdfIndexSub = []
                }
        first_chapter = pdfIndexPage (head real_index)

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
unfoldElem l (this:sub) = PDFIndex {
        pdfIndexTitle = title this,
        pdfIndexPage = page this,
        pdfIndexSub = unfoldElems (l+1) sub
        }

unfoldElems :: Int -> [[Fields]] -> [PDFIndex]
unfoldElems l = map (unfoldElem l) . groupBy (\_ n -> (level n > l))

parseDumpData :: String -> PDFData
parseDumpData = putInShape .  partition isIndexField . catMaybes . map getField . lines

getPDFInfo :: FilePath -> IO PDFData
getPDFInfo file = do
        let options = [file,"dump_data"]
        (inp, out, err, pid) <- runInteractiveProcess "pdftk" options Nothing Nothing
        hClose inp
        hClose err
        info <- hGetContents out
        -- klappt leider nicht
        -- forkIO $ waitForProcess pid >> return ()
        return $ parseDumpData info

formatPDFInfo :: String -> PDFData -> [DocElement]
formatPDFInfo file info = [
        Header 3 (B.pack "PDF-Index"),
        Paragraph $ [Text $ B.pack $ "This PDF-File has "++show(numberOfPages info)++" pages"]] ++
        if not ( null (pdfIndex info) ) then
                [ItemList $  map formatPDFIndex $ zip [1..] $ pdfIndex info ]
        else
                []

 where  formatPDFIndex (n, (PDFIndex title page sub)) = [
                Text (B.pack title),
                Text (B.singleton ' '),
                LinkElem $ PlainLink (B.pack (subfile n)) $ B.pack $ "(Page "++ show page++")"
                ]
        subfile n = takeFileName $ chapterFile file n

extractPDFPages infile outfile (from, to) = do
        let range = (show from) ++ "-" ++ (show to)
        let options = [infile,"cat",range,"output",outfile]
        -- liftIO $ print $ unwords ("pdftk":options)
        pid <- runProcess "pdftk" options Nothing Nothing Nothing Nothing Nothing
        waitForProcess pid
        return ()

ranges n [] = []
ranges n (x:xs) | pdfIndexPage x > 1 = (1, pdfIndexPage x - 1) : ranges' n (x:xs)
                | otherwise          =                           ranges' n (x:xs)
ranges' n [] = []
ranges' n [last] =   [(pdfIndexPage last, n)]
ranges' n (x:y:xs) = (pdfIndexPage x,next) : ranges' n (y:xs)
  where next = max (pdfIndexPage x) (pdfIndexPage y - 1)

chapterFile file n = file ++ "." ++ (show n) ++ ".pdf"

splitPDF file info = do
        let n        = numberOfPages info
            chapters = zip [1..] (ranges n (pdfIndex info))
        flip mapM_ chapters $ \(n,r) -> do
                let outfile = chapterFile file n
                liftIO $ extractPDFPages file outfile r
