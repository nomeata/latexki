module WikiData where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Time

newtype PageName = PageName String deriving (Eq, Ord)
instance Show PageName where
 show (PageName pn) = "Page " ++ pn

data PageInfo = PageInfo {
        smPageName  :: PageName,
        smType      :: String,
        smContent   :: B.ByteString,
        smModTime   :: UTCTime
}
instance Eq PageInfo where
 p1 == p2 = smPageName p1 == smPageName p2
instance Ord PageInfo where
 p1 `compare` p2 = smPageName p1 `compare` smPageName p2
instance Show PageInfo where
 show p1 = "PageInfo " ++ show (smPageName p1)

data LectureInfo = LectureInfo {
    liFil :: PageInfo,
    liMetaData :: Maybe MetaData
    }
    deriving (Show)

data PDFIndex = PDFIndex { pdfIndexTitle :: String, pdfIndexPage :: Int, pdfIndexSub :: [PDFIndex] }
    deriving (Show, Read)
data PDFData = PDFData { numberOfPages :: Int, pdfIndex :: [PDFIndex] }
    deriving (Show, Read)

data TexIndex = TexIndex {
    tiTitle :: B.ByteString,
    tiPageFrom :: Int,
    tiPageTo :: Int}
    deriving (Show, Read)

data MetaData = MetaData {
    mdTitle :: B.ByteString,
    mdLecturer :: Maybe B.ByteString,
    mdSemester :: Maybe B.ByteString,
    mdState :: Maybe B.ByteString,
    mdIndex :: [TexIndex],
    mdLastChange :: RawLogEntry,
    mdPDFData :: Maybe PDFData
    }
    deriving (Show, Read)

type RecentChanges = [LogEntry]
type RawRecentChanges = [RawLogEntry]

data RawLogEntry = RawLogEntry {
        hashR :: B.ByteString,
        authorR :: B.ByteString,
        dateR :: ZonedTime,
        pathsR :: [String],
        messageR :: B.ByteString
        }
    deriving (Read, Show)

data LogEntry = LogEntry {
        hash :: B.ByteString,
        author :: B.ByteString,
        date :: ZonedTime,
        links :: [Link],
        message :: InlineText,
        websvn :: Maybe Link } 
    deriving (Show)

data Link =     WikiLink PageInfo B.ByteString |
                NewLink String |
                DLLink B.ByteString |
                PlainLink B.ByteString B.ByteString deriving Show

data InlineTextElem =   Text B.ByteString |
                        LinkElem Link |
                        Image B.ByteString B.ByteString deriving Show

type InlineText = [InlineTextElem] 

data DocElement = Paragraph InlineText | EnumList [InlineText] | 
                  ItemList [InlineText] | PreFormat B.ByteString |
                  Header Int B.ByteString | RCElem RecentChanges | 
                  LIElem LectureInfo | LectureSearch |
                  HLine
    deriving Show
                

type Document = [DocElement]  
