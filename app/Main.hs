{-# LANGUAGE OverloadedStrings #-}

module Main where

{-
- annotations on deposit -- why are they necessary?
- add a persistent main
- lower <-- precedence?
- no thedb global, pass it through
- Node monad: collect writes, then apply them sequentially
  - Instead of applying the writes, collect them?
  - Get rid of all explicit mentions of db; top level 'nmain' should be inside the node monad and runNode or whatever passes in the db, then saves the resulting
  modified db
- bidi mmap
- ====
- Terse notation? F or --> (can have --> even if no Func), V; what about <-- for write???
- bidi head, tail, cons
- Can you do TMI.., as in TMI.(.)
- fromList? OverloadedLists extension + IsList
- Rename to hide orig stuff and rename node stuff to look orig
- How do features translate to node-lifted world?
- How are errors
- Read about lenses
- Use lenses, applicative like god intended

arrlookup_f :: Int -> [a] -> a
arrlookup_b :: Int -> a -> [a] -> [a]

-}

import Control.Applicative
import Control.Monad.State
import Data.Function
import qualified Data.Map.Strict as M
import Data.String (IsString(..))
import qualified Debug.Trace as TR
import System.IO
import Util 

type History = [DB]
data DB = DB { a :: Int, b :: [Int], c :: String, accounts :: M.Map String Int }
  deriving (Eq, Read, Show)

thedb = DB { a = 12, b = [2, 3, 4], c = "asdf", accounts = M.fromList [] }

data Val b = Val (DB -> b) (b -> DB -> DB)

vconst v = uni $ const v

for (Val f r) = f
rev (Val f r) = r

norev = error "norev"

uni f = Val f norev

vread = for

theroot = Val id const

liftV :: (a -> b) -> Val a -> Val b
liftV f = liftBV f norev
liftV2 :: (a -> b -> c) -> Val a -> Val b -> Val c
liftV2 f = liftBV2 f norev
liftBV :: (a -> b) -> (b -> a -> a) -> Val a -> Val b
liftBV f b x = Val nf nb
  where nf db = f (for x db)
        nb ny db = rev x (b ny (for x db)) db
liftBV2 :: (a -> b -> c) -> (c -> (a, b) -> (a, b)) -> Val a -> Val b -> Val c
liftBV2 f b bbb ccc = Val fd bd
  where fd x = f (for bbb x) (for ccc x)
        bd nv x = let (nb, nc) = b nv (for bbb x, for ccc x)
                   in rev ccc nc (rev bbb nb x)

vsp v = msp $ vread v thedb

vwrite :: Val a -> Val a -> DB -> DB
vwrite (Val f b) v a = b (vread v a) a

-- bidi inc
binc :: Val Int -> Val Int
binc = liftBV (+1) (\i _ -> i-1)

-- Bidirectional additition: in the reverse direction, spread the change
-- between the two inputs.  So forward 1 + 1 = 2 ; reverse 4 = 2 + 2
bidiPlus :: Val Int -> Val Int -> Val Int
bidiPlus = liftBV2 (\x y -> x + y) rev
  where rev nsum (ox, oy) = (nx, ny)
          where osum = ox + oy
                delta = nsum - osum
                nx = ox + (delta `div` 2)
                ny = nsum - nx

instance Num a => Num (Val a) where
  (+) = liftV2 (+)
  (*) = liftV2 (*)
  abs = liftV abs
  signum = liftV signum
  fromInteger i = uni $ const $ fromInteger i
  negate = liftV negate

instance IsString a => IsString (Val a) where
  fromString s = uni $ const $ fromString s

ntrue = vconst True
nfalse = vconst False

nif :: Val Bool -> Val b -> Val b -> Val b
nif c ~t ~e = uni f
  where f db = if (for c db) then (for t db) else (for e db)

neq :: Eq b => Val b -> Val b -> Val Bool
neq = liftV2 (==)

nhead :: Val [b] -> Val b
nhead = liftV head

ntail :: Val [b] -> Val [b]
ntail = liftV tail

ncons :: Val b -> Val [b] -> Val [b]
ncons = liftV2 (:)

nmap :: (Eq a, Show a) => (Val a -> Val b) -> Val [a] -> Val [b]
nmap f as = nif (neq as (vconst []))
                (vconst [])
                (ncons (f (nhead as)) (nmap f (ntail as)))

nmap2 = liftV2 map

nmain = do
  hSetBuffering stdin NoBuffering
  msp "hi"
  msp $ vread theroot thedb
  --vsp theroot
  vsp $ _a
  vsp $ (liftV (+ 10)) $ _a
  vsp $ (liftV2 (+)) _a (_bi 1)
  msp $ vwrite _a 120 thedb
  massert $ (vwrite _a 120 thedb) == DB { a = 120 , b = [ 2 , 3 , 4 ] , c = "asdf", accounts = M.fromList [] } 
  vsp $ binc $ _a
  massert $ (vwrite (binc $ _a) 130 thedb) ==
    DB { a = 129 , b = [ 2 , 3 , 4 ] , c = "asdf", accounts = M.fromList [] } 
  vsp $ _a `bidiPlus` (_bi 1)
  msp $ vwrite (_a `bidiPlus` (_bi 1)) 19 thedb
  massert $ (vwrite (_a `bidiPlus` (_bi 1)) 19 thedb) ==
    DB { a = 14 , b = [ 2 , 5 , 4 ] , c = "asdf", accounts = M.fromList [] }
  let floo :: Val Int
      floo = 123
  vsp floo
  msp "mult"
  msp $ vwrite (_bi 1) 335 $ vwrite _a 126 $ vwrite _c "zxcv" thedb
  massert $ (vwrite (_bi 1) 335 $ vwrite _a 126 $ vwrite _c "zxcv" thedb) ==
    DB { a = 126 , b = [ 2 , 335 , 4 ] , c = "zxcv", accounts = M.fromList [] }
  vsp $ nmap (liftV (\x -> x * 2)) (vconst [1, 2, 3])
  vsp $ nmap2 (vconst (\x -> x * 2)) (vconst [1, 2, 3])
  massert $ (vread (nmap (liftV (\x -> x * 2)) (vconst [1, 2, 3])) thedb) == [2, 4, 6]
  massert $ (vread (nmap2 (vconst (\x -> x * 2)) (vconst [1, 2, 3])) thedb) == [2, 4, 6]
  vsp $ floo `neq` 120
  vsp $ floo `neq` 123
  vsp $ nif (vconst True) "istrue" "isfalse"
  vsp $ nif (vconst False) "istrue" "isfalse"
  vsp $ nif ntrue "istrue" "isfalse"
  vsp $ nif nfalse "istrue" "isfalse"
  vsp $ neq ntrue ntrue
  vsp $ neq ntrue nfalse
  vsp $ neq 12 12
  vsp $ neq 12 13
  vsp $ liftV (*10) 13
  vsp $ nhead (vconst [20, 21, 22])
  vsp $ ntail (vconst [20, 21, 22])
  vsp $ nhead $ ntail (vconst [20, 21, 22])
  vsp $ ncons 19 (vconst [20, 21, 22])
  vsp $ ncons 19 $ ntail (vconst [20, 21, 22])
  vsp $ (+ 10) _a
  vsp $ (+) _a (_bi 1)
  vsp $ _a + 10
  vsp $ 10 + _a
  vsp $ _a + (_bi 1)

up_a v db = db { a = v }
up_b v db = db { b = v }
up_c v db = db { c = v }
up_accounts :: M.Map String Int -> DB -> DB
up_accounts v db = db { accounts = v }
_a :: Val Int
_a = liftBV a up_a theroot
_b = liftBV b up_b theroot
_c = liftBV c up_c theroot
_accounts = liftBV accounts up_accounts theroot
_i :: Int -> Val [a] -> Val a
_i i = liftBV (!! i) (\nv oarr -> upd oarr i nv)
_m_f :: Ord a => a -> M.Map a b -> b
_m_f k m = m M.! k
_m_b :: Ord a => a -> b -> M.Map a b -> M.Map a b
_m_b k v m = M.insert k v m
_m k = liftBV (_m_f k) (_m_b k)
upd :: [a] -> Int -> a -> [a]
upd as i a
  | i < 0 || i >= length as = error "upd out of range"
  | otherwise = (take i as) ++ [a] ++ (drop (i+1) as)
--_bi i = uni $ \arr -> b arr !! i
_bi :: Int -> Val Int
_bi i = (_i i) _b

type Write = DB -> DB
mkwrite :: Val a -> Val a -> Write
mkwrite = vwrite

type TMI a = StateT [Write] IO (Val a)

(<--) :: Val a -> Val a -> TMI ()
dest <-- src = do
  writes <- get
  put $ writes ++ [mkwrite dest src]
  return $ vconst ()

io = liftIO

foo :: TMI Int
foo = do
  io $ msp "ho"
  _a <-- 120
  return _a

applyWrites :: [Write] -> DB -> DB
applyWrites writes db = foldl (&) db writes

tmiRun :: DB -> TMI a -> IO (a, DB)
tmiRun db action = do
  msp "hi"
  (x, writes) <- runStateT action []
  let result = vread x db
      newDb = applyWrites writes db
  msp newDb
  return (result, newDb)

persistentRun :: TMI a -> IO a
persistentRun action = do
  historyS <- readFile "history.db"
  let history :: History
      history = (read historyS) :: History
  (result, newDb) <- tmiRun (last history) action
  let newHistory = history ++ [newDb]
      newHistoryS = show newHistory
  writeFile "history.db" newHistoryS
  return result

ofmain = tmiRun thedb foo

fmain = do
  result <- persistentRun foo
  msp result

processLines:: String -> (String -> IO ()) -> IO ()
processLines filename action = do
  bankCommand <- openFile filename ReadMode
  let loop = do
      eof <- hIsEOF bankCommand
      if eof
        then return ()
        else do
                line <- hGetLine bankCommand
                action line
                loop
  loop

processBankCommandString :: String -> IO ()
processBankCommandString line = persistentRun $ processBankCommand (words line)

processBankCommand :: [String] -> TMI ()
--processBankCommand command = do
  --_accounts <-- (vconst $ M.fromList [("bar", 20)])
  --((_m "baz") _accounts) <-- (vconst 30)
processBankCommand ["createAccount", name] = do
  ((_m name) _accounts) <-- (vconst 0)
processBankCommand ["deposit", name, amount] = do
  let currentBalance :: Val Int
      currentBalance = ((_m name) _accounts)
      deposit :: Val Int
      deposit = vconst $ read amount
      newBalance :: Val Int
      newBalance = currentBalance + deposit
  ((_m name) _accounts) <-- newBalance

bankProcess = processLines "bank-commands.txt" processBankCommandString

main = do bankProcess
