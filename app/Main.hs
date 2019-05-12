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
- write map using nodes
- combinators for those basic elements
- f db
- fromList?
- reversible map?  nif neq, how??
- two kinds of nodes?  db -> b and a -> b
- write a sort using nodes
- norev constructor (uni)
- Node monad: collect writes, then apply them sequentially
- Get rid of all explicit mentions of db; top level 'nmain' should be inside the node monad and runNode or whatever passes in the db, then saves the resulting
  modified db
- N
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
import Util 

data DB = DB { a :: Int, b :: [Int], c :: String }
  deriving (Eq, Read, Show)

norev = undefined

--root db = FNode id rid
  --where rid db _ = db

data FNode a b = FNode (a -> b) (b -> a -> a)

ncompose :: FNode b c -> FNode a b -> FNode a c
ncompose (FNode fbc bbc) (FNode fab bab) = FNode fac bac
  where fac a = (fbc . fab) a
        bac c oa = let ob = (fab oa)
                       nb = bbc c ob
                       na = bab nb oa
                    in na

fshow :: Show b => FNode a b -> a -> String
fshow (FNode f b) a = show $ f a

fnread :: FNode a b -> a -> b
fnread (FNode f b) a = f a

instance Num b => Num (FNode a b) where
  (+) (FNode fa _) (FNode fb _) = FNode (\a -> fa a + fb a) norev
  (*) (FNode fa _) (FNode fb _ ) = FNode (\a -> fa a * fb a) norev
  abs (FNode f _) = FNode (\a -> abs $ f a) norev
  signum (FNode f _) = FNode (\a -> signum $ f a) norev
  fromInteger i = FNode (\_ -> fromInteger i) norev
  negate (FNode f _) = FNode (\a -> negate $ f a) norev

instance IsString b => IsString (FNode a b) where
  fromString s = FNode (\_ -> fromString s) norev

_a :: FNode DB Int
_a = FNode (\db -> a db) (\v db -> db { a = v })
_b = FNode (\db -> b db) (\v db -> db { b = v })
_c = FNode (\db -> c db) (\v db -> db { c = v })
_i :: Int -> FNode [a] a
_i i = FNode (\arr -> arr !! i) (\nv oarr -> upd oarr i nv)
upd :: [a] -> Int -> a -> [a]
upd as i a
  | i < 0 || i >= length as = error "upd out of range"
  | otherwise = (take i as) ++ [a] ++ (drop (i+1) as)
_bi i = FNode (\arr -> b arr !! i) norev
_bi' i = ncompose (_i i) _b

write :: FNode a b -> FNode a b -> a -> a
write (FNode f b) v a = b (fnread v a) a

nequal (FNode fa _) (FNode fb _) = FNode (\a -> (fa a) == (fb a)) norev

nsp n = msp $ fnread n db

db = DB { a = 12, b = [2, 3, 4], c = "asdf" }

nconst :: b -> FNode a b
nconst x = FNode (\_ -> x) norev

ntrue = nconst True
nfalse = nconst False

nif :: FNode a Bool -> FNode a b -> FNode a b -> FNode a b
nif (FNode fc _) (FNode ft _) (FNode fe _) = FNode f norev
  where f = \db -> if (fc db) then (eesp "then" (ft db)) else (eesp "else" (fe db))
        --b = norev

neq :: Eq b => FNode a b -> FNode a b -> FNode a Bool
neq (FNode fa _) (FNode fb _) = FNode f norev
  where f = \db -> (fa db) == (fb db)

napply :: FNode a (b -> c) -> FNode a b -> FNode a c
napply (FNode ff _) (FNode fb _) = FNode f norev
  where f = \db -> (ff db) (fb db)

nhead :: FNode a [b] -> FNode a b
nhead (FNode fa _) = FNode f norev
  where f = \db -> head (fa db)

ntail :: FNode a [b] -> FNode a [b]
ntail (FNode fa _) = FNode f norev
  where f = \db -> tail (fa db)
{- bad safe tail
  where f = \db -> let foo = (fa db)
                    in if (null foo) -- foo == []
                         then []
                         else tail foo
-}

ncons :: FNode a b -> FNode a [b] -> FNode a [b]
ncons (FNode fb _) (FNode fbs _) = FNode f norev
  where f = \db -> (fb db) : (fbs db)

{-`
mymap :: Eq a => (a -> b) -> [a] -> [b]
mymap f as =
  if as == []
    then []
    else (f (head as)) : (mymap f (tail as))
-}

--nmap :: Eq a => FNode d (a -> b) -> FNode d [a] -> FNode d [b]
nmap :: (Eq a, Show a) => FNode DB (a -> b) -> FNode DB [a] -> FNode DB [b]
nmap f as | TR.trace (show ("nmap", fnread (neq as (nconst [])) db, fnread as db)) False = undefined
nmap f as = nif (neq as (nconst []))
                (eesp "reca" (nconst []))
                (eesp ("rec", fnread as db) (ncons (napply f (nhead as)) (nmap f (ntail as))))
                --(eesp ("rec", fnread as db) (ncons (napply f (nhead as)) (nmap f (nconst []))))
                --(eesp ("rec", fnread as db) (ncons (napply f (nhead as)) (nconst [])))

{-
nmap :: Eq a => FNode d (a -> b) -> FNode d [a] -> FNode d [b]
nmap f@(FNode ff _) as@(FNode fas _) = FNode f norev
  where f = \db -> let as = fas db
                    in if as == []
                         then (const [])
                          else let b = napply 
                                in ncons b bs
-}

main = do
  msp "hi"
  let fnoo :: FNode DB Int
      fnoo = 121
  nsp fnoo
  nsp _a
  nsp _b
  nsp _c
  nsp $ _bi 1
  nsp $ _bi' 1
  msp $ write _a fnoo db
  msp $ write _a 122 db
  msp $ write _c "zxcv" db
  msp $ write (_bi' 1) 333 db
  msp $ write (_bi' 1) 334 $ write _a 123 $ write _c "zxcv" db
  massert $ (write (_bi' 1) 335 $ write _a 123 $ write _c "zxcv" db) == DB { a = 123 , b = [ 2 , 335 , 4 ] , c = "zxcv" }
  nsp $ fnoo `nequal` 120
  nsp $ fnoo `nequal` 121
  --msp $ mymap (\x -> x * 2) [1, 2, 3]
  --nsp $ nif (FNode True norev) (FNode "istrue" norev) (FNode "isfalse" norev)
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
  --nsp $ nmap (nconst (\x -> x * 2)) (nconst [1, 2, 3])
