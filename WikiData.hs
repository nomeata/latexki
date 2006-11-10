module WikiData where

type RecentChanges = [LogEntry]
type RawRecentChanges = [RawLogEntry]

data RawLogEntry = RawLogEntry { revisionR :: Int, authorR :: String, dateR :: String, pathsR :: [String], messageR :: String } 

data LogEntry = LogEntry { revision :: Int, author :: String, date :: String, links :: [Link], message :: InlineText } 

data Link = Link String String [String] | NewLink String | DLLink String | PlainLink String String

data InlineTextElem = Text String | LinkElem Link | Image String String

type InlineText = [InlineTextElem] 

data DocElement = Paragraph InlineText | EnumList [InlineText] | 
                  ItemList [InlineText] | PreFormat String |
		  Header Int String | RCElem RecentChanges | HLine

type Document = [DocElement]
