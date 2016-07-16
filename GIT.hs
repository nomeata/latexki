{-# LANGUAGE RecordWildCards #-}
module GIT (getGitRecentChanges, getGitLastChange) where

import System.Process
import System.IO
import Control.Concurrent
import qualified Data.ByteString.Lazy.Char8 as B
import System.FilePath
import Data.Time
import Text.Parsec
import Text.Parsec.String
import Data.Functor
import Data.List

import WikiData
import Common

lineRest0 = many (noneOf "\n") <* newline
lineRest = many1 (noneOf "\n") <* newline

parseLogEntry :: TimeZone -> Parser RawLogEntry
parseLogEntry tz = do
    string "commit" >> many1 space
    hashR <- B.pack <$> many1 hexDigit
    endOfLine
    optional $ string "Merge:" >> lineRest
    string "Author:" >> many1 space
    authorR <- B.pack <$> lineRest
    string "Date:" >> many1 space
    dateR <- readGitTime tz <$> lineRest
    endOfLine
    messageR <- B.pack . unlines <$> many1 (string "    " >> lineRest0)
    endOfLine
    pathsR <- filter (not . ("." `isPrefixOf`)) <$> many lineRest
    return $ RawLogEntry {..}

readGitTime :: TimeZone -> String -> ZonedTime
readGitTime tz = utcToZonedTime tz . parseTimeOrError True defaultTimeLocale rfc822DateFormat


parseLogEntries tz = sepBy1 (parseLogEntry tz) endOfLine

wholeFile p = p <* eof

runGitCommand :: [String] -> IO String
runGitCommand args = readProcess "git" (["-C",datadir] ++ args) ""

getGitRecentChanges :: IO [RawLogEntry]
getGitRecentChanges = do
   log <- runGitCommand ["log","-n","10","--format=medium", "--name-only","--date=rfc2822"]
   tz <- getCurrentTimeZone
   return $
    either (\e -> error (show e ++ "\n" ++ log)) id $
    parse (wholeFile (parseLogEntries tz)) "git output" log

getGitLastChange :: FilePath -> IO RawLogEntry
getGitLastChange file = do
   log <- runGitCommand ["log","-n","1","--format=medium", "--name-only", "--date=rfc2822", "--", file]
   tz <- getCurrentTimeZone
   return $
    either (\e -> error (show e ++ "\n" ++ log)) id $
    parse (wholeFile (parseLogEntry tz)) "git output" log
