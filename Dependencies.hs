module Dependencies (
	needUpdate, needUpdates, isUpToDate, DepResult(..), showState,
	anyOlder, isOlder
) where

import Data.Monoid
import System
import System.IO
import System.Posix.IO
import Directory
import Common
import WikiData

data DepResult = UpToDate | ResultMissing PageInfo | DepNew [(PageInfo, PageInfo)] | Always String 

isUpToDate UpToDate = True
isUpToDate _        = False

showState name UpToDate = return ()
showState name x        = putStrLn $ name ++ " updated because: " -- ++ (show x)

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


needUpdates targets deps = mconcat `fmap` mapM (flip needUpdate deps) targets

needUpdate target deps = do
	let targetTime = getInputTime target
	let needUpdate' dep = do
		let sourceTime = getInputTime dep
		return $ if sourceTime < targetTime then
			UpToDate
		   else 
			DepNew [(dep,target)]
	mconcat `fmap` mapM needUpdate' deps
		 	

anyOlder page targets = or `fmap` mapM (isOlder page) targets

isOlder page target = do
	let sourceTime = Just $ getInputTime page
	targetTime <- getOutputTime target
	return $ sourceTime > targetTime
