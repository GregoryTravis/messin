module Main where

import Control.Exception (try)
import qualified Data.Map.Strict as M
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
runMut Mut{ mutStep  = mutStep } = mutStep

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

--instance Monad (Mut s) where
  --(>>=) = composeMutsV

main :: IO ()
main = do
  let m :: M.Map String Int
      m = M.empty
  let ((), m') = runMut (setMut "a" 5) m
  msp m
  msp m'
  let (5, _) = runMut (getMut "a") m'
  x <- muts
  let ((), m'') = runMut (incMut "a") m'
  msp m''
  msp x
