{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances  #-}

module CacheT (CacheT,runCacheT, callCache) where

import Data.Map 
import Control.Monad.State

class (Monad m, Ord a) => CacheMonad a b m | m -> a b where
	callCache :: a -> m b

data CacheState a b m = CacheState { cacheF :: (a -> m b), cacheM :: Map a b }

newtype CacheT a b m r = CacheT { unwrapCacheT :: CacheState a b m -> m (r, CacheState a b m) }

instance (Monad m) => Monad (CacheT a b m) where
	return a = CacheT $ \s -> return (a,s)
	m >>= k  = CacheT $ \s -> do
		(a, s') <- unwrapCacheT m s
		unwrapCacheT (k a) s'
	fail str = CacheT $ \_ -> fail str

runCacheT :: (Monad m, Ord a) => (a -> m b) -> CacheT a b m r -> m r
runCacheT f m = do
	fst `liftM` (unwrapCacheT m) (CacheState f empty)

instance (Monad m, Ord a) => CacheMonad a b (CacheT a b m)  where
	callCache a = CacheT $ \(CacheState f map) -> case Data.Map.lookup a map of
		 Nothing -> do
			v <- f a
			let map' = insert a v map
			return (v, CacheState f map')
		 Just v -> return (v, CacheState f map)
		
instance MonadTrans (CacheT a b) where
	lift m = CacheT $ \s -> do
		a <- m
		return (a, s)

instance (MonadIO m) => MonadIO (CacheT a b m) where
	liftIO = lift . liftIO


