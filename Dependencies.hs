module Dependencies (
	needUpdate, needUpdates, isUpToDate, DepResult(..), showState,
	anysOlder, anyOlder, isOlder
) where

import Data.Monoid
import System
import System.IO
import System.Posix.IO
import Directory
import Common
import WikiData

data DepResult =
	UpToDate |
--	ResultMissing PageInfo |
	DepNew [(PageInfo, PageInfo)] |
	Always String deriving (Show)

isUpToDate UpToDate = True
isUpToDate _        = False

showState name UpToDate = return ()
showState name x        = putStrLn $ name ++ " updated because: "  ++ (show x)

instance Monoid DepResult where
	mempty = UpToDate

	Always r1       `mappend` Always r2       = Always (r1 ++ r2)
	DepNew l1       `mappend` DepNew l2       = DepNew (l1 ++ l2)
	Always r        `mappend` _               = Always r
	_               `mappend` Always r        = Always r
--	ResultMissing r `mappend` _               = ResultMissing r
--	_               `mappend` ResultMissing r = ResultMissing r
	UpToDate        `mappend` anything        = anything
	anything        `mappend` UpToDate        = anything


needUpdates targets deps = concat $ map (`needUpdate` deps) targets

needUpdate target deps = concat $ flip map deps $ \dep ->
			if getInputTime dep > getInputTime target then [dep] else []

anysOlder pages targets = or `fmap` mapM (flip anyOlder targets) pages

anyOlder page targets = or `fmap` mapM (isOlder page) targets

isOlder page target = do
	let sourceTime = Just $ getInputTime page
	targetTime <- getOutputTime target
	return $ sourceTime > targetTime
