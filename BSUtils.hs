module BSUtils (subStringCI) where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char
import Control.Monad
import Prelude 

subStringCI :: B.ByteString -> B.ByteString -> Bool
subStringCI pat str = not $ null $ do
	n <- B.findIndices (\c -> toLower c == start) str	
	guard $ all (\(c1,c2) -> c1 == toLower c2) $ B.zip pat' (B.drop n str)
  where pat'  = B.map (toLower) pat
 	start = B.head pat'
