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

norev = undefined

uni f = Node f norev

--root db = Node id rid
  --where rid db _ = db

data Node a b = Node (a -> b) (b -> a -> a)

ncompose :: Node b c -> Node a b -> Node a c
ncompose (Node fbc bbc) (Node fab bab) = Node fac bac
  where fac a = (fbc . fab) a
        bac c oa = let ob = (fab oa)
                       nb = bbc c ob
                       na = bab nb oa
                    in na

fshow :: Show b => Node a b -> a -> String
fshow (Node f b) a = show $ f a

fnread :: Node a b -> a -> b
fnread (Node f b) a = f a

instance Num b => Num (Node a b) where
  (+) (Node fa _) (Node fb _) = uni $ \a -> fa a + fb a
  (*) (Node fa _) (Node fb _ ) = uni $ \a -> fa a * fb a
  abs (Node f _) = uni $ \a -> abs $ f a
  signum (Node f _) = uni $ \a -> signum $ f a
  fromInteger i = uni $ \_ -> fromInteger i
  negate (Node f _) = uni $ \a -> negate $ f a

instance IsString b => IsString (Node a b) where
  fromString s = uni $ \_ -> fromString s

_a :: Node DB Int
_a = Node (\db -> a db) (\v db -> db { a = v })
_b = Node (\db -> b db) (\v db -> db { b = v })
_c = Node (\db -> c db) (\v db -> db { c = v })
_i :: Int -> Node [a] a
_i i = Node (\arr -> arr !! i) (\nv oarr -> upd oarr i nv)
upd :: [a] -> Int -> a -> [a]
upd as i a
  | i < 0 || i >= length as = error "upd out of range"
  | otherwise = (take i as) ++ [a] ++ (drop (i+1) as)
_bi i = uni $ \arr -> b arr !! i
_bi' i = ncompose (_i i) _b

write :: Node a b -> Node a b -> a -> a
write (Node f b) v a = b (fnread v a) a

nequal (Node fa _) (Node fb _) = uni $ \a -> (fa a) == (fb a)

nsp n = msp $ fnread n thedb

thedb = DB { a = 12, b = [2, 3, 4], c = "asdf" }

nconst :: b -> Node a b
nconst x = uni $ const x

ntrue = nconst True
nfalse = nconst False

nif :: Node a Bool -> Node a b -> Node a b -> Node a b
nif (Node fc _) ~(Node ft _) ~(Node fe _) = uni f
  where f db = if (fc db) then (ft db) else (fe db)

neq :: Eq b => Node a b -> Node a b -> Node a Bool
neq (Node fa _) (Node fb _) = uni f
  where f db = (fa db) == (fb db)

napply :: Node a (b -> c) -> Node a b -> Node a c
napply (Node ff _) (Node fb _) = uni f
  where f db = (ff db) (fb db)

nhead :: Node a [b] -> Node a b
nhead (Node fa _) = uni f
  where f db = head (fa db)

ntail :: Node a [b] -> Node a [b]
ntail (Node fa _) = uni f
  where f db = tail (fa db)
{- bad safe tail
  where f db = let foo = (fa db)
                    in if (null foo) -- foo == []
                         then []
                         else tail foo
-}

ncons :: Node a b -> Node a [b] -> Node a [b]
ncons (Node fb _) (Node fbs _) = uni f
  where f db = (fb db) : (fbs db)

{-`
mymap :: Eq a => (a -> b) -> [a] -> [b]
mymap f as =
  if as == []
    then []
    else (f (head as)) : (mymap f (tail as))
-}

--nmap :: Eq a => Node d (a -> b) -> Node d [a] -> Node d [b]
nmap :: (Eq a, Show a) => Node DB (a -> b) -> Node DB [a] -> Node DB [b]
nmap f as = nif (neq as (nconst []))
                (nconst [])
                (ncons (napply f (nhead as)) (nmap f (ntail as)))

main = do
  hSetBuffering stdin NoBuffering
  msp "hi"
  let fnoo :: Node DB Int
      fnoo = 121
  nsp fnoo
  nsp _a
  nsp _b
  nsp _c
  nsp $ _bi 1
  nsp $ _bi' 1
  msp $ write _a fnoo thedb
  msp $ write _a 122 thedb
  msp $ write _c "zxcv" thedb
  msp $ write (_bi' 1) 333 thedb
  msp $ write (_bi' 1) 334 $ write _a 123 $ write _c "zxcv" thedb
  massert $ (write (_bi' 1) 335 $ write _a 123 $ write _c "zxcv" thedb) == DB { a = 123 , b = [ 2 , 335 , 4 ] , c = "zxcv" }
  nsp $ fnoo `nequal` 120
  nsp $ fnoo `nequal` 121
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
