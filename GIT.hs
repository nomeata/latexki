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
lineRestZ = manyTill anyChar (char '\0')

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
    pathsR <- option [] $ do
        endOfLine
        filter (not . ("." `isPrefixOf`)) <$> many lineRestZ
    return $ RawLogEntry {..}

readGitTime :: TimeZone -> String -> ZonedTime
readGitTime tz = utcToZonedTime tz . parseTimeOrError True defaultTimeLocale rfc822DateFormat


parseLogEntries tz = sepBy1 (parseLogEntry tz) (char '\0')

wholeFile p = p <* eof

runGitCommand :: [String] -> IO String
runGitCommand args = readProcess "git" (["-C",datadir] ++ args) ""

runGitLog :: [String] -> IO String
runGitLog args = runGitCommand (["log","--format=medium", "-z", "--name-only","--date=rfc2822"] ++ args)

getGitRecentChanges :: IO [RawLogEntry]
getGitRecentChanges = do
   log <- runGitLog ["-n","10"]
   tz <- getCurrentTimeZone
   return $
    either (\e -> error (show e ++ "\n" ++ log)) id $
    parse (wholeFile (parseLogEntries tz)) "git output" log

getGitLastChange :: FilePath -> IO RawLogEntry
getGitLastChange file = do
   log <- runGitLog ["-n","1","--", file]
   tz <- getCurrentTimeZone
   return $
    either (\e -> error (show e ++ "\n" ++ log)) id $
    parse (wholeFile (parseLogEntry tz)) "git output" log
