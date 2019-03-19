module Main where

import Control.Exception (try)
import qualified Data.Map.Strict as M
import Data.Typeable
import System.IO (getLine, putStrLn)
import System.IO.Error (isEOFError)

import Util

muts :: IO (Maybe (String, String))
muts = do
  input <- try getLine
  case input of 
    Left e -> if isEOFError e then return Nothing else ioError e
    Right line -> do
      let [cmd, arg] = words line
      return $ Just (cmd, arg)

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

newtype MutT m s a = MutT { mutTStep :: s -> m (a, s) }
setMutT :: (Monad m, Ord k) => k -> v -> MutT m (M.Map k v) ()
setMutT k v = MutT { mutTStep = \s -> return ((), M.insert k v s) }
getMutT :: (Monad m, Ord k) => k -> MutT m (M.Map k v) v
getMutT k = MutT { mutTStep = \s -> return (s M.! k, s) }
runMutT :: MutT m s a -> s -> m (a, s)
runMutT MutT{ mutTStep = mutTStep } = mutTStep

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
  voo2 <- runMutT (setMutT "a" 50) M.empty
  msp voo2
  --let x :: s -> IO ((), M.Map String Int)
      --x = runMutT (setMutT "a" 50)
  --tsp x

  msp "hi"
  --x <- muts
  --msp x

main = withMutT
