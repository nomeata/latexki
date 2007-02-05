module WikiData where

type RecentChanges = [LogEntry]
type RawRecentChanges = [RawLogEntry]

data RawLogEntry = RawLogEntry { revisionR :: Int, authorR :: String, dateR :: String, pathsR :: [String], messageR :: String }  deriving Show

data LogEntry = LogEntry { revision :: Int, author :: String, date :: String, links :: [Link], message :: InlineText }  deriving Show

data Link = WikiLink String String | NewLink String | DLLink String | PlainLink String String deriving Show

data InlineTextElem = Text String | LinkElem Link | Image String String deriving Show

type InlineText = [InlineTextElem] 

data DocElement = Paragraph InlineText | EnumList [InlineText] | 
                  ItemList [InlineText] | PreFormat String |
		  Header Int String | RCElem RecentChanges | HLine deriving Show
		

type Document = [DocElement] 
