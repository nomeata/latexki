module Dependencies (needUpdate, needUpdates, isUpToDate, DepResult(..), showState) where

import Data.Monoid
import System
import System.IO
import System.Posix.IO
import Directory

data DepResult = UpToDate | ResultMissing String | DepNew [(String, String)] | Always String deriving (Show)

isUpToDate UpToDate = True
isUpToDate _        = False

showState name UpToDate = return ()
showState name x        = putStrLn $ name ++ " updated because: " ++ (show x)

instance Monoid DepResult where
	mempty = UpToDate

	UpToDate        `mappend` anything        = anything
	anything        `mappend` UpToDate        = anything
	Always r1       `mappend` Always r2       = Always (r1 ++ r2)
	Always r        `mappend` _               = Always r
	_               `mappend` Always r        = Always r
	ResultMissing r `mappend` _               = ResultMissing r
	_               `mappend` ResultMissing r = ResultMissing r
	DepNew l1       `mappend` DepNew l2       = DepNew (l1 ++ l2)


needUpdates outputs deps = mconcat `fmap` mapM (flip needUpdate deps) outputs

needUpdate output deps = do
	ex <- doesFileExist output
	if not ex then
		return $ ResultMissing output
	   else do
	  	targetTime <- getModificationTime output
              	let needUpdate' dep = do
			ex <- doesFileExist dep
			if not ex then
				-- This is a strange case and probably should be
				-- handled specially or just fail
				return UpToDate
			   else do
				sourceTime <- getModificationTime dep
				return $ if sourceTime < targetTime then
					UpToDate
				   else 
				   	DepNew [(dep,output)]
		mconcat `fmap` mapM needUpdate' deps
		 	

