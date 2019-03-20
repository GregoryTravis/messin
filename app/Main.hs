module Main where

import Control.Exception (try)
import qualified Data.Map.Strict as M
import Data.Typeable
import System.IO (getLine, putStrLn)
import System.IO.Error (isEOFError)

import Util

{-
muts :: IO (Maybe (String, String))
muts = do
  input <- try getLine
  case input of 
    Left e -> if isEOFError e then return Nothing else ioError e
    Right line -> do
      let [cmd, arg] = words line
      return $ Just (cmd, arg)
-}

newtype Mut s a = Mut { mutStep :: s -> (a, s) }
composeMuts :: Mut s a -> Mut s b -> Mut s b
composeMuts Mut{ mutStep = a } Mut{ mutStep = b } = Mut { mutStep = c }
  where c s = case a s of (x, s') -> b s
composeMutsV :: Mut s a -> (a -> Mut s b) -> Mut s b
composeMutsV Mut{ mutStep = a } f = Mut { mutStep = c }
  where c s = case a s of (x, s') -> case f x of Mut{ mutStep = b } -> b s'
setMut :: Ord k => k -> v -> Mut (M.Map k v) ()
setMut k v = Mut { mutStep = \m -> ((), M.insert k v m) }
getMut :: Ord k => k -> Mut (M.Map k v) v
getMut k = Mut { mutStep = \m -> (m M.! k, m) }
runMut :: Mut s a -> s -> (a, s)
runMut Mut{ mutStep = mutStep } = mutStep

incMut :: (Ord k, Num v) => k -> Mut (M.Map k v) ()
incMut k = composeMutsV (getMut k) (\n -> setMut k (n + 1))

instance Functor (Mut s) where
  -- fmap :: (a -> b) -> Mut s a -> Mut s b
  fmap f Mut{mutStep=mutStep} = Mut{mutStep=c}
    where c s = case mutStep s of (x, s') -> (f x, s')

instance Applicative (Mut s) where
  -- pure :: a -> Mut s a
  pure a = Mut{mutStep = \s -> (a, s)}
  -- (<*>) :: Mut s (a -> b) -> Mut s a -> Mut s b
  Mut{mutStep=f} <*> Mut{mutStep=a} = Mut{mutStep=c}
    where c s = case f s of (f, s') -> case a s' of (x, s'') -> (f x, s'')

instance Monad (Mut s) where
  (>>=) = composeMutsV

withMut = do
  let m :: M.Map String Int
      m = M.empty
  let ((), m') = runMut (setMut "a" 5) m
  --msp m
  msp m'
  let (5, _) = runMut (getMut "a") m'
  let ((), m'') = runMut (incMut "a") m'
  --msp m''
  let ((), m''') = runMut (getMut "a" >>= \n -> setMut "a" (n+1)) m''
  --msp m'''
  let h :: Mut (M.Map String Int) ()
      h = do n <- getMut "a"
             setMut "a" (n+1)
  --msp $ typeOf h
  let q = runMut h
  --msp $ typeOf q
  let ((), m'''') = q m'''
  --msp $ typeOf m''''
  --msp m''''
  let ((), m''''') = runMut (do n <- getMut "a"
                                setMut "a" (n+1)) m''''
  --msp $ typeOf m'''''
  msp m'''''

{-
  let fee = (runMutT :: MutT IO (M.Map String Int) () -> (M.Map String Int) -> IO ((), (M.Map String Int)))
  ttsp fee
  let arg = (setMutT "a" 50) :: MutT IO (M.Map String Int) ()
  ttsp arg
  let app1 = fee arg
  ttsp app1
  let arg2 = M.empty :: (M.Map String Int)
  ttsp arg2
  let app2 = app1 arg2
  ttsp app2
  voo <- app2
  tsp voo
  --voo2 <- runMutT (setMutT "a" (50::Int)) M.empty
-}

withMutT = do
  ((), m) <- runMutT M.empty (setMutT "a" 50)
  msp m
  (50, m') <- runMutT m (getMutT "a")
  msp m'
  massert $ m == m'
  ((), m'') <- runMutT m' (incMutT "a")
  msp m''
  --let x :: s -> IO ((), M.Map String Int)
      --x = runMutT (setMutT "a" 50)
  --tsp x

  msp "hi"
  --x <- muts
  --msp x

newtype MutT m s a = MutT { mutTStep :: s -> m (a, s) }
composeMutTsV :: (Monad m) => MutT m s a -> (a -> MutT m s b) -> MutT m s b
composeMutTsV MutT{ mutTStep = a } f = MutT { mutTStep = c }
  --where c s = case a s of (x, s') -> case f x of Mut{ mutStep = b } -> return $ b s'
  where c s = do (x, s') <- a s
                 case f x of MutT{ mutTStep = b } -> b s'
setMutT :: (Monad m, Ord k) => k -> v -> MutT m (M.Map k v) ()
setMutT k v = MutT { mutTStep = \s -> return ((), M.insert k v s) }
getMutT :: (Monad m, Ord k) => k -> MutT m (M.Map k v) v
getMutT k = MutT { mutTStep = \s -> return (s M.! k, s) }
incMutT :: (Monad m, Ord k, Num v) => k -> MutT m (M.Map k v) ()
incMutT k = composeMutTsV (getMutT k) (\n -> setMutT k (n + 1))
runMutT :: (Monad m) => s -> MutT m s a -> m (a, s)
runMutT s MutT{ mutTStep = mutTStep } = mutTStep s
--mspInMutT :: (Monad m) => String -> MutT m (M.Map k v) ()
--mspInMutT s = MutT{mutTStep = \s -> (do msp s ; return ((), s))}

instance Monad m => Functor (MutT m s) where
  -- fmap :: (a -> b) -> Mut s a -> Mut s b
  fmap f MutT{mutTStep=mutTStep} = MutT{mutTStep=c}
    --where c s = case mutTStep s of (x, s') -> return (f x, s')
    where c s = do (x, s') <- mutTStep s
                   return (f x, s')

instance Monad m => Applicative (MutT m s) where
  -- pure :: a -> MutT s a
  pure a = MutT{mutTStep = \s -> return (a, s)}
  -- (<*>) :: MutT s (a -> b) -> MutT s a -> MutT s b
  MutT{mutTStep=f} <*> MutT{mutTStep=a} = MutT{mutTStep=c}
    --where c s = case f s of (f, s') -> case a s' of (x, s'') -> (f x, s'')
    where c s = do (f, s') <- f s
                   (x, s'') <- a s'
                   return (f x, s'')

instance Monad m => Monad (MutT m s) where
  (>>=) = composeMutTsV

monadly = do
  let x = setMutT "a" 50 :: MutT IO (M.Map String Int) ()
  let y = incMutT "a" :: MutT IO (M.Map String Int) ()
  --let z = x >>= \s -> y
  let x' :: IO ((), M.Map String Int)
      x' = runMutT M.empty $ do setMutT "a" 50
                                --msp "ho"
                                incMutT "a"
  ((), m) <- runMutT M.empty $ do setMutT "a" 50
                                  --msp "ho"
                                  incMutT "a"
  msp m

newtype MutIOT s a = MutIOT { mutIOTStep :: s -> IO (a, s) }
composeMutIOTsV :: MutIOT s a -> (a -> MutIOT s b) -> MutIOT s b
composeMutIOTsV MutIOT{ mutIOTStep = a } f = MutIOT { mutIOTStep = c }
  --where c s = case a s of (x, s') -> case f x of Mut{ mutStep = b } -> return $ b s'
  where c s = do (x, s') <- a s
                 case f x of MutIOT{ mutIOTStep = b } -> b s'
setMutIOT :: Ord k => k -> v -> MutIOT (M.Map k v) ()
setMutIOT k v = MutIOT { mutIOTStep = \s -> return ((), M.insert k v s) }
mspMutIOT :: String -> MutIOT (M.Map k v) ()
mspMutIOT str = MutIOT { mutIOTStep = \s -> do { msp str ; return ((), s) } }
runMutIOT :: s -> MutIOT s a -> IO (a, s)
runMutIOT s MutIOT{ mutIOTStep = mutIOTStep } = mutIOTStep s
liftMutIOT :: IO a -> MutIOT (M.Map k v) a
liftMutIOT io = MutIOT{mutIOTStep = step}
  where step s = do a <- io
                    return (a, s)

instance Functor (MutIOT s) where
  -- fmap :: (a -> b) -> Mut s a -> Mut s b
  fmap f MutIOT{mutIOTStep=mutIOTStep} = MutIOT{mutIOTStep=c}
    --where c s = case mutTStep s of (x, s') -> return (f x, s')
    where c s = do (x, s') <- mutIOTStep s
                   return (f x, s')

instance Applicative (MutIOT s) where
  pure a = MutIOT{mutIOTStep = \s -> return (a, s)}
  MutIOT{mutIOTStep=f} <*> MutIOT{mutIOTStep=a} = MutIOT{mutIOTStep=c}
    where c s = do (f, s') <- f s
                   (x, s'') <- a s'
                   return (f x, s'')

instance Monad (MutIOT s) where
  (>>=) = composeMutIOTsV

monadlyIO = do
  ((), m) <- runMutIOT M.empty $ do setMutIOT "a" 50
                                    mspMutIOT "gosh2"
                                    liftMutIOT $ msp "gosh3"
                                    --msp "ho"
                                    --incMutIOT "a"
  msp "oh"

monadlyNoDo =
  -- x, y ok because of annotations
  let x = setMutT "a" 50 :: MutT IO (M.Map String Int) ()
      y = incMutT "a" :: MutT IO (M.Map String Int) ()
      -- nope, needs annotation
      -- y' = incMutT "a"
      -- Doesn't work without this type annotation
      x' :: IO ((), M.Map String Int)
      x' = runMutT M.empty $ do setMutT "a" 50
                                --msp "ho"
                                incMutT "a"
   in 1

main = do
  --withMut
  --withMutT
  --monadly
  --msp monadlyNoDo
  monadlyIO
