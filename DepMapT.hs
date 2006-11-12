module DepMapT (genDepMap, Dependency(Dep,Special)) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map (Map, (!))
import Data.Set (Set)

import Monad
import Maybe

data Dependency a b = Dep a | Special b deriving (Ord, Eq)

toMaybe (Dep d)     = Just d
toMaybe (Special _) = Nothing

genDepMap  :: (Ord a, Ord b, Monad m) => [a] -> (a -> m [Dependency a b]) -> m (Map a [Dependency a b ])
genDepMap cands deps = (M.map S.toList . trans)  `liftM` nextPlease (cands, M.empty)
  where nextPlease ([],m) = return m
	nextPlease (c:rest,m) = if c `M.member` m
		then
			nextPlease (rest,m)
		else do
			nc <- deps c
			let todo = mapMaybe toMaybe nc
			nextPlease (rest ++ todo, M.insert c (S.fromList nc) m)

trans m = M.map (conMapSet trans') m 
  where -- why does this type definition give errors?
  	--trans' :: (Ord a) => a -> Set a
  	trans' (Special s) = S.singleton (Special s)
  	trans' (Dep e)     = let d = m ! e in S.union d (conMapSet trans' (S.filter (/= (Dep e)) d))


conMapSet :: (Ord a) => (a -> Set a) -> Set a ->  Set a
conMapSet f = S.unions . S.toList . S.map f
