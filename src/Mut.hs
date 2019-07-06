module Mut (monadly) where

import Control.Exception (try)
import Control.Monad (ap, liftM)
import qualified Data.Map.Strict as M
import Data.Typeable
import System.IO (getLine, putStrLn)
import System.IO.Error (isEOFError)

import Util

newtype MutT m s a = MutT (s -> m (a, s))
setMutT :: (Monad m, Ord k) => k -> v -> MutT m (M.Map k v) ()
setMutT k v = MutT (\s -> return ((), M.insert k v s))
getMutT :: (Monad m, Ord k) => k -> MutT m (M.Map k v) v
getMutT k = MutT (\s -> return (s M.! k, s))
incMutT :: (Monad m, Ord k, Num v) => k -> MutT m (M.Map k v) ()
incMutT k = (getMutT k) >>= (\n -> setMutT k (n + 1))
runMutT :: (Monad m) => s -> MutT m s a -> m (a, s)
runMutT s (MutT pl) = pl s
liftMutT :: (Monad m) => m a -> MutT m (M.Map k v) a
liftMutT action = MutT $ \s -> do a <- action
                                  return (a, s)
{-
instance Monad m => Functor (MutT m s) where
  -- fmap :: (a -> b) -> Mut s a -> Mut s b
  fmap f (MutT pl) = MutT c
    --where c s = case mutTStep s of (x, s') -> return (f x, s')
    where c s = do (x, s') <- pl s
                   return (f x, s')

instance Monad m => Applicative (MutT m s) where
  -- pure :: a -> MutT s a
  pure a = MutT (\s -> return (a, s))
  -- (<*>) :: MutT s (a -> b) -> MutT s a -> MutT s b
  MutT f <*> MutT a = MutT c
    --where c s = case f s of (f, s') -> case a s' of (x, s'') -> (f x, s'')
    where c s = do (f, s') <- f s
                   (x, s'') <- a s'
                   return (f x, s'')
-}

instance Monad m => Applicative (MutT m s) where
    pure = return
    (<*>) = ap

instance Monad m => Functor (MutT m s) where
    fmap = liftM

instance Monad m => Monad (MutT m s) where
  -- return :: (Monad M) => a -> Mut m s a
  -- return a = MutT (\s -> return (a, s))
  (MutT a) >>= f = MutT $ \s -> (a s) >>= \(x, s') -> case f x of MutT b -> b s'

monadly = do
  let x = setMutT "a" 50 :: MutT IO (M.Map String Int) ()
  let y = incMutT "a" :: MutT IO (M.Map String Int) () --let z = x >>= \s -> y
  let b' :: MutT IO (M.Map String Int) ()
      b' = do setMutT "a" 60
              liftMutT $ msp "gosh4"
              incMutT "a"
  let x' :: IO ((), M.Map String Int)
      x' = runMutT M.empty $ do setMutT "a" 60
                                liftMutT $ msp "gosh4"
                                incMutT "a"
  ((), m) <- runMutT M.empty $ do setMutT "a" 60
                                  liftMutT $ msp "gosh4"
                                  liftMutT $ msp "gosh5"
                                  liftMutT $ do msp "gosh6"
                                                msp "gosh7"
                                  incMutT "a"
  msp m
  ((), m') <- runMutT m $
                (getMutT "a") >>=
                (\n -> setMutT "a" (n + 1)) >>
                (liftMutT $ msp "gosh10")
  msp m'
  let bb' :: MutT IO (M.Map String Int) ()
      bb' = MutT (\s -> return (s M.! "a", s)) >>=
            \n -> MutT (\s -> return ((), M.insert "a" (n+1) s)) >>
            MutT (\s -> do a <- msp "gosh11"
                           return (a, s))
  let q :: IO ((), M.Map String Int)
      q = runMutT m' $
                 MutT (\s -> return (s M.! "a", s)) >>=
                 \n -> MutT (\s -> return ((), M.insert "a" (n+1) s)) >>=
                 \() -> MutT (\s -> do a <- msp "gosh11"
                                       return (a, s))
  ((), m'') <- runMutT m' $
                 MutT (\s -> return (s M.! "a", s)) >>=
                 \n -> MutT (\s -> return ((), M.insert "a" (n+1) s)) >>=
                 \() -> MutT (\s -> do a <- msp "gosh11"
                                       return (a, s))
  msp m''
  ((), m''') <- runMutT m'' $
                 (MutT (\s -> do (x, s') <- (\s -> return (s M.! "a", s)) s
                                 (\n -> \s -> return ((), M.insert "a" (n+1) s)) x s')) >>
                 (liftMutT $ msp "gosh12")
  msp m'''
