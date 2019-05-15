{-# LANGUAGE OverloadedStrings #-}

module Main where

{-
+ Node as wrapped DB -> a
+ DB field accessor node constructors
+ Make it a tuple
+ backwards function
+ clean up
+ write takes a node instead of a raw value (necessary to not have to pass db everywhere)
+ string literal too
x NEq
+ nequal
+ generic, not DB
+ maybe a b instead of b a
+ change db -> a
+ _bi should be a composition
+ _bi rev
+ write map using nodes
+ norev constructor (uni)
+ N / Node
+ liftV* in terms of liftBV*
x val, func, sfunc -- builders
+ if uni always has toVal before it?
+ shouldn't need nid
  + Is there a way for a node to only have the b type?
+ lifters, obvs
+ reverse lifters
+ f db
+ two kinds of nodes?  db -> b and a -> b
- clean up
- currying?
- bidi mmap
- Node monad: collect writes, then apply them sequentially
  - Get rid of all explicit mentions of db; top level 'nmain' should be inside the node monad and runNode or whatever passes in the db, then saves the resulting
  modified db
- ====
- Terse notation? F or -->, V
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
import Data.String (IsString(..))
import qualified Debug.Trace as TR
import System.IO
import Util 

data DB = DB { a :: Int, b :: [Int], c :: String }
  deriving (Eq, Read, Show)

thedb = DB { a = 12, b = [2, 3, 4], c = "asdf" }

data Func a b = Func (a -> b) (b -> a -> a)
newtype Val b = Val (Func DB b)

vconst v = Val (nconst v)
nconst :: b -> Func a b
nconst x = uni $ const x

napply' :: Func a b -> Val a -> Val b
napply' (Func ffor frev) (Val (Func vfor vrev)) = Val (Func nvfor nvrev)
  where nvfor db = ffor (vfor db)
        nvrev b db = vrev (frev b (vfor db)) db

for (Func f b) = f
rev (Func f b) = b
vfor (Val func) = for func
vrev (Val func) = rev func

norev = error "norev"

-- This is just weird
nid = Func id const

uni f = Func f norev
toUni (Func f b) = Func f norev

ncompose :: Func b c -> Func a b -> Func a c
ncompose f@(Func fbc bbc) g@(Func fab bab) = Func fac bac
  where fac a = (for f . for g) a
        bac c oa = rev g (rev f c (for g oa)) oa

fshow :: Show b => Func a b -> a -> String
fshow (Func f b) a = show $ f a

fnread :: Func a b -> a -> b
fnread (Func f b) a = f a

vread (Val func) = fnread func

theroot = Val nid

liftV :: (a -> b) -> Val a -> Val b
liftV f = liftBV f norev
liftV2 :: (a -> b -> c) -> Val a -> Val b -> Val c
liftV2 f = liftBV2 f norev
liftBV :: (a -> b) -> (b -> a -> a) -> Val a -> Val b
liftBV f r a = napply' (Func f r) a
liftBV2 :: (a -> b -> c) -> (c -> (a, b) -> (a, b)) -> Val a -> Val b -> Val c
liftBV2 f b bbb ccc = Val (Func fd bd)
  where fd x = f (vfor bbb x) (vfor ccc x)
        bd nv x = let (nb, nc) = b nv (vfor bbb x, vfor ccc x)
                   in vrev ccc nc (vrev bbb nb x)

vsp v = msp $ vread v thedb

fwrite :: Func a b -> Func a b -> a -> a
fwrite (Func f b) v a = b (fnread v a) a
vwrite :: Val a -> Val a -> DB -> DB
vwrite (Val func) (Val v) = fwrite func v

-- bidi inc
binc :: Val Int -> Val Int
binc = liftBV (+1) (\i _ -> i-1)
--
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
  fromInteger i = Val $ uni $ const $ fromInteger i
  negate = liftV negate

instance IsString a => IsString (Val a) where
  fromString s = Val $ uni $ const $ fromString s

ntrue = vconst True
nfalse = vconst False

nif :: Val Bool -> Val b -> Val b -> Val b
nif c ~t ~e = Val $ uni f
  where f db = if (vfor c db) then (vfor t db) else (vfor e db)

neq :: Eq b => Val b -> Val b -> Val Bool
neq = liftV2 (==)

--napply :: Val (b -> c) -> Val b -> Val c
--napply = liftV2 ($)

nhead :: Val [b] -> Val b
nhead = liftV head

ntail :: Val [b] -> Val [b]
ntail = liftV tail

ncons :: Val b -> Val [b] -> Val [b]
ncons = liftV2 (:)

{-
mymap :: Eq a => (a -> b) -> [a] -> [b]
mymap f as =
  if as == []
    then []
    else (f (head as)) : (mymap f (tail as))
-}

nmap :: (Eq a, Show a) => Func a b -> Val [a] -> Val [b]
nmap f as = nif (neq as (vconst []))
                (vconst [])
                (ncons (napply' f (nhead as)) (nmap f (ntail as)))

nmap2 = liftV2 map

main = do
  hSetBuffering stdin NoBuffering
  msp "hi"
  msp $ vread theroot thedb
  --vsp theroot
  vsp $ _a
  vsp $ (liftV (+ 10)) $ _a
  vsp $ (liftV2 (+)) _a (_bi 1)
  msp $ vwrite _a (vconst 120) thedb
  massert $ (vwrite _a (vconst 120) thedb) == DB { a = 120 , b = [ 2 , 3 , 4 ] , c = "asdf" } 
  vsp $ binc $ _a
  massert $ (vwrite (binc $ _a) (vconst 130) thedb) ==
    DB { a = 129 , b = [ 2 , 3 , 4 ] , c = "asdf" } 
  vsp $ _a `bidiPlus` (_bi 1)
  msp $ vwrite (_a `bidiPlus` (_bi 1)) (vconst 19) thedb
  massert $ (vwrite (_a `bidiPlus` (_bi 1)) (vconst 19) thedb) ==
    DB { a = 14 , b = [ 2 , 5 , 4 ] , c = "asdf" }
  let floo :: Val Int
      floo = 123
  vsp floo
  msp $ vwrite (_bi 1) 335 $ vwrite _a 126 $ vwrite _c "zxcv" thedb
  massert $ (vwrite (_bi 1) 335 $ vwrite _a 123 $ vwrite _c "zxcv" thedb) == DB { a = 123 , b = [ 2 , 335 , 4 ] , c = "zxcv" }
  vsp $ nmap (uni (\x -> x * 2)) (vconst [1, 2, 3])
  vsp $ nmap2 (vconst (\x -> x * 2)) (vconst [1, 2, 3])
  massert $ (vread (nmap (uni (\x -> x * 2)) (vconst [1, 2, 3])) thedb) == [2, 4, 6]
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
  vsp $ napply' (uni $ \x -> x*10) 13
  vsp $ nhead (vconst [20, 21, 22])
  vsp $ ntail (vconst [20, 21, 22])
  vsp $ nhead $ ntail (vconst [20, 21, 22])
  vsp $ ncons 19 (vconst [20, 21, 22])
  vsp $ ncons 19 $ ntail (vconst [20, 21, 22])

up_a v db = db { a = v }
up_b v db = db { b = v }
up_c v db = db { c = v }
_a :: Val Int
_a = liftBV a up_a theroot
_b = liftBV b up_b theroot
_c = liftBV c up_c theroot
_i :: Int -> Val [a] -> Val a
_i i = liftBV (!! i) (\nv oarr -> upd oarr i nv)
upd :: [a] -> Int -> a -> [a]
upd as i a
  | i < 0 || i >= length as = error "upd out of range"
  | otherwise = (take i as) ++ [a] ++ (drop (i+1) as)
--_bi i = uni $ \arr -> b arr !! i
_bi :: Int -> Val Int
_bi i = (_i i) _b
