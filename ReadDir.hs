module ReadDir (
	DirEntry(..),
	readDir,
) where

import System.FilePath
import System.Directory
import Control.Monad
import qualified Data.ByteString as B

data DirEntry = DirEntry { 
	deFileName :: FilePath,
--	deModTime  :: ClockTime,
	deFileContent :: B.ByteString
}

--readDir :: FilePath -> IO [DirEntry]
readDir :: FilePath -> IO [FilePath]
readDir dir = map (makeRelative dir) `liftM` recursive dir
  where recursive dir = do
		all_entries <- getDirectoryContents dir
		let entries = filter (\(h:_) -> h /= '.') all_entries
		files  <- filterM (doesFileExist . combine dir) entries
		dirs   <- filterM (doesDirectoryExist . combine dir) entries
		deeper <- concat `liftM` mapM (recursive . combine dir) dirs
		return $ map (combine dir) (files ++ deeper)




