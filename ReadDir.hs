module ReadDir (
	DirEntry(..),
	readDir,
) where

import System.FilePath hiding (makeRelative)
import System.Directory
import System.IO.Unsafe
import System.Time
import Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB

data DirEntry = DirEntry { 
	deFileName :: FilePath,
	deModTime  :: ClockTime,
	deFileContent :: LB.ByteString
} deriving (Show)

readDir :: FilePath -> IO [DirEntry]
readDir dir = mapM mkDirEntry =<< map (makeRelative dir) `liftM` recursive dir
  where	mkDirEntry file = do
  		modTime <- {- unsafeInterleaveIO $ -} getModificationTime (dir </> file)
  		content <-    unsafeInterleaveIO $  (LB.fromChunks . (:[])) `liftM` B.readFile (dir </> file)
		return $ DirEntry {
			deFileName = file,
			deModTime  = modTime,
			deFileContent = content }
  	recursive dir = do
		all_entries <- if dir == "" 
			then getDirectoryContents "."
			else getDirectoryContents dir
		let entries = filter (\(h:_) -> h /= '.') all_entries
		files  <- filterM (doesFileExist . combine dir) entries
		dirs   <- filterM (doesDirectoryExist . combine dir) entries
		deeper <- concat `liftM` mapM (recursive . combine dir) dirs
		return $ map (combine dir) (files ++ deeper)




makeRelative :: FilePath -> FilePath -> FilePath
makeRelative cur x = joinPath $
                         replicate (length curdir - common) ".." ++
                         drop common orgpth
    where
        common = length $ takeWhile id $ zipWith (==) orgdir curdir
        orgpth = splitPath pth
        orgdir = splitDirectories pth
        curdir = splitDirectories $ normalise $ cur
	pth =  normalise x
