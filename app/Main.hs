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
- val, func, sfunc -- builders
- if uni always has toVal before it?
- And <--, <--- builders
- Func and Val, Val hidden ctor, toFunc Val; use newtype for Val if it can be hidden, or not
  - want to make sure you can't pass a val as func so maybe just different types with a converter
  - which is just (id, id)
  - Func should actually be (-->) because we need to use these things in actual code
  - Which also suggests F and V; but really a preprocessor will be necessary I guess
- bidi head, tail, cons
- Can you do TMI.., as in TMI.(.)
- mmap
- mmap -> nodemap
- shouldn't need nid
  - Is there a way for a node to only have the b type?
+ lifters, obvs
+ reverse lifters
- combinators for those basic elements
+ f db
- fromList? OverloadedLists extension + IsList
- reversible map?  nif neq, how??
- two kinds of nodes?  db -> b and a -> b
- Node monad: collect writes, then apply them sequentially
- Use infix instead of N/Node/Node, like fclabels :->
- Get rid of all explicit mentions of db; top level 'nmain' should be inside the node monad and runNode or whatever passes in the db, then saves the resulting
  modified db
- currying?
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

toVal :: Func DB b -> Val b
toVal func = Val func

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

norev = undefined

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
liftV f a = napply' (uni f) a
liftV2 :: (a -> b -> c) -> Val a -> Val b -> Val c
liftV2 f a b = toVal $ uni $ \db -> f (vfor a db) (vfor b db)
liftBV :: (a -> b) -> (b -> a -> a) -> Val a -> Val b
liftBV f r a = napply' (Func f r) a
liftBV2 :: (a -> b -> c) -> (c -> (a, b) -> (a, b)) -> Val a -> Val b -> Val c
liftBV2 f b bbb ccc = Val (Func fd bd)
  where fd x = f (vfor bbb x) (vfor ccc x)
        bd nv x = let (nb, nc) = b nv (vfor bbb x, vfor ccc x)
                   in vrev ccc nc (vrev bbb nb x)

vsp v = msp $ vread v thedb

vwrite :: Val a -> Val a -> DB -> DB
vwrite (Val func) (Val v) = write func v

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
  fromInteger i = toVal $ uni $ const $ fromInteger i
  negate = liftV negate

instance IsString a => IsString (Val a) where
  fromString s = toVal $ uni $ const $ fromString s

ntrue = vconst True
nfalse = vconst False

nif :: Val Bool -> Val b -> Val b -> Val b
nif c ~t ~e = toVal $ uni f
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
  vsp $ toVal _a
  vsp $ (liftV (+ 10)) $ toVal _a
  vsp $ (liftV2 (+)) (toVal _a) (toVal (_bi 1))
  msp $ vwrite (toVal _a) (vconst 120) thedb
  massert $ (vwrite (toVal _a) (vconst 120) thedb) == DB { a = 120 , b = [ 2 , 3 , 4 ] , c = "asdf" } 
  vsp $ binc $ toVal _a
  massert $ (vwrite (binc $ toVal _a) (vconst 130) thedb) ==
    DB { a = 129 , b = [ 2 , 3 , 4 ] , c = "asdf" } 
  vsp $ (toVal _a) `bidiPlus` (toVal (_bi 1))
  msp $ vwrite ((toVal _a) `bidiPlus` (toVal (_bi 1))) (vconst 19) thedb
  massert $ (vwrite ((toVal _a) `bidiPlus` (toVal (_bi 1))) (vconst 19) thedb) ==
    DB { a = 14 , b = [ 2 , 5 , 4 ] , c = "asdf" }
  let floo :: Val Int
      floo = 123
  vsp floo
  msp $ vwrite (toVal (_bi 1)) 335 $ vwrite (toVal _a) 126 $ vwrite (toVal _c) "zxcv" thedb
  massert $ (vwrite (toVal (_bi 1)) 335 $ vwrite (toVal _a) 123 $ vwrite (toVal _c) "zxcv" thedb) == DB { a = 123 , b = [ 2 , 335 , 4 ] , c = "zxcv" }
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
_a :: Func DB Int
--_a = Func (\db -> a db) (\v db -> up_a v db)
_a = liftBN a up_a nid
_b :: Func DB [Int]
_b = liftBN b up_b nid
_c = liftBN c up_c nid
--_b = Func (\db -> b db) (\v db -> db { b = v })
--_c = Func (\db -> c db) (\v db -> db { c = v })
_i :: Int -> Func [a] a
_i i = liftBN (!! i) (\nv oarr -> upd oarr i nv) nid
--_i i = Func (\arr -> arr !! i) (\nv oarr -> upd oarr i nv)
upd :: [a] -> Int -> a -> [a]
upd as i a
  | i < 0 || i >= length as = error "upd out of range"
  | otherwise = (take i as) ++ [a] ++ (drop (i+1) as)
--_bi i = uni $ \arr -> b arr !! i
_bi :: Int -> Func DB Int
_bi i = ncompose (_i i) _b

write :: Func a b -> Func a b -> a -> a
write (Func f b) v a = b (fnread v a) a

liftN :: (b -> c) -> Func a b -> Func a c
liftN f n = toUni $ liftBN f undefined n

liftN2 :: (b -> c -> d) -> Func a b -> Func a c -> Func a d
liftN2 f n0 n1 = toUni $ liftBN2 f undefined n0 n1

liftBN :: (b -> c) -> (c -> b -> b) -> Func a b -> Func a c
liftBN f b (Func fa ba) = Func (\x -> f (fa x)) (\v x -> (ba (b v (fa x)) x))

liftBN2 :: (b -> c -> d) -> (d -> (b, c) -> (b, c)) -> Func a b -> Func a c -> Func a d
liftBN2 f b bbb ccc = Func fd bd
  where fd x = f (for bbb x) (for ccc x)
        bd nv x = let (nb, nc) = b nv (for bbb x, for ccc x)
                   in rev ccc nc (rev bbb nb x)

{-
{-
instance Num a => Num (Val a) where
  (+) = liftN2 (+)
  (*) = liftN2 (*)
  abs = liftN abs
  signum = liftN signum
  fromInteger i = uni $ const $ fromInteger i
  negate = liftN negate
-}

instance Num b => Num (Func a b) where
  (+) = liftN2 (+)
  (*) = liftN2 (*)
  abs = liftN abs
  signum = liftN signum
  fromInteger i = uni $ const $ fromInteger i
  negate = liftN negate

-- Bidirectional additition: in the reverse direction, spread the change
-- between the two inputs.  So forward 1 + 1 = 2 ; reverse 4 = 2 + 2
bidiPlus :: Func a Int -> Func a Int -> Func a Int
bidiPlus = liftBN2 (\x y -> x + y) rev
  where rev nsum (ox, oy) = (nx, ny)
          where osum = ox + oy
                delta = nsum - osum
                nx = ox + (delta `div` 2)
                ny = nsum - nx

instance IsString b => IsString (Func a b) where
  fromString s = uni $ const $ fromString s

nsp n = msp $ fnread n thedb

ntrue = nconst True
nfalse = nconst False

nif :: Func a Bool -> Func a b -> Func a b -> Func a b
nif (Func fc _) ~(Func ft _) ~(Func fe _) = uni f
  where f db = if (fc db) then (ft db) else (fe db)

neq :: Eq b => Func a b -> Func a b -> Func a Bool
neq = liftN2 (==)

napply :: Func a (b -> c) -> Func a b -> Func a c
napply = liftN2 ($)

nhead :: Func a [b] -> Func a b
nhead = liftN head

ntail :: Func a [b] -> Func a [b]
ntail = liftN tail

ncons :: Func a b -> Func a [b] -> Func a [b]
ncons = liftN2 (:)

{-`
mymap :: Eq a => (a -> b) -> [a] -> [b]
mymap f as =
  if as == []
    then []
    else (f (head as)) : (mymap f (tail as))
-}

--nmap :: Eq a => Func d (a -> b) -> Func d [a] -> Func d [b]
nmap :: (Eq a, Show a) => Func DB (a -> b) -> Func DB [a] -> Func DB [b]
nmap f as = nif (neq as (nconst []))
                (nconst [])
                (ncons (napply f (nhead as)) (nmap f (ntail as)))

nmap2 = liftN2 map

{-
mmap :: Func b c -> Func a [b] -> Func a [c]
mmap f = liftBN for rev
  where for = map (for f)
        rev xs oxs = map (\(x, ox) -> rev f x) $ zip xs oxs
-}

_main = do
  hSetBuffering stdin NoBuffering
  msp "hi"
  let fnoo :: Func DB Int
      fnoo = 121
      nfnoo = (-121)
  nsp fnoo
  nsp $ fnoo + 1000
  nsp $ fnoo * 2
  nsp $ abs nfnoo
  nsp $ signum nfnoo
  nsp $ negate nfnoo
  nsp _a
  nsp _b
  nsp _c
  nsp $ _bi 1
  nsp $ _bi 1
  msp $ write _a fnoo thedb
  msp $ write _a 122 thedb
  msp $ write _c "zxcv" thedb
  msp $ write (_bi 1) 333 thedb
  msp $ write (_bi 1) 334 $ write _a 123 $ write _c "zxcv" thedb
  massert $ (write (_bi 1) 335 $ write _a 123 $ write _c "zxcv" thedb) == DB { a = 123 , b = [ 2 , 335 , 4 ] , c = "zxcv" }
  nsp $ fnoo `neq` 120
  nsp $ fnoo `neq` 121
  nsp $ nif (nconst True) "istrue" "isfalse"
  nsp $ nif (nconst False) "istrue" "isfalse"
  nsp $ nif ntrue "istrue" "isfalse"
  nsp $ nif nfalse "istrue" "isfalse"
  nsp $ neq ntrue ntrue
  nsp $ neq ntrue nfalse
  nsp $ neq 12 12
  nsp $ neq 12 13
  nsp $ napply (nconst $ \x -> x*10) 13
  nsp $ nhead (nconst [20, 21, 22])
  nsp $ ntail (nconst [20, 21, 22])
  nsp $ nhead $ ntail (nconst [20, 21, 22])
  nsp $ ncons 19 (nconst [20, 21, 22])
  nsp $ ncons 19 $ ntail (nconst [20, 21, 22])
  nsp $ nmap (nconst (\x -> x * 2)) (nconst [1, 2, 3])
  nsp $ nmap2 (nconst (\x -> x * 2)) (nconst [1, 2, 3])
  nsp $ _a + (_bi 1)
  nsp $ _a `bidiPlus` (_bi 1)
  msp $ write (_a `bidiPlus` (_bi 1)) 19 thedb
  massert $ (write (_a `bidiPlus` (_bi 1)) 19 thedb) == DB { a = 14 , b = [ 2 , 5 , 4 ] , c = "asdf" }
-}
