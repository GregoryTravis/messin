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
- generic, not DB
- maybe a b instead of b a
- _bi should be a composition
- _bi rev
- write a sort using ncompare
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

arrlookup_f :: Int -> [a] -> a
arrlookup_b :: Int -> a -> [a] -> [a]

-}

import Control.Applicative
import Data.String (IsString(..))
import Util 

data DB = DB { a :: Int, b :: [Int], c :: String }
  deriving (Read, Show)

norev = undefined

--root db = FNode id rid
  --where rid db _ = db

data FNode b a = FNode (b -> a) (a -> b -> b)

ncompose :: FNode c b -> FNode b a -> FNode c a
ncompose (FNode fcb bcb) (FNode fba bba) = FNode fca bca
  where -- fca :: c -> a
        fca c = fba (fcb c)
        -- fcb :: c -> b
        -- fba :: b -> a
        -- bcb :: b -> c -> c
        -- bba :: a -> b -> b
        -- bca :: a -> c -> c
        bca a oc = let ob = (fcb oc)
                       nb = bba a ob
                       nc = bcb nb oc
                    in nc

fshow :: Show a => FNode b a -> b -> String
fshow (FNode f b) db = show $ f db

fnread :: FNode b a -> b -> a
fnread (FNode f b) db = f db

instance Num a => Num (FNode b a) where
  (+) (FNode fa _) (FNode fb _) = FNode (\db -> fa db + fb db) norev
  (*) (FNode fa _) (FNode fb _ ) = FNode (\db -> fa db * fb db) norev
  abs (FNode f _) = FNode (\db -> abs $ f db) norev
  signum (FNode f _) = FNode (\db -> signum $ f db) norev
  fromInteger i = FNode (\db -> fromInteger i) norev
  negate (FNode f _) = FNode (\db -> negate $ f db) norev

instance IsString a => IsString (FNode b a) where
  fromString s = FNode (\db -> fromString s) norev

_a :: FNode DB Int
_a = FNode (\db -> a db) (\v db -> db { a = v })
_b = FNode (\db -> b db) norev
_c = FNode (\db -> c db) (\v db -> db { c = v })
_i :: Int -> FNode [a] a
_i i = FNode (\arr -> arr !! i) norev
_bi i = FNode (\db -> b db !! i) norev
_bi' i = ncompose _b (_i i)

write :: FNode b a -> FNode b a -> b -> b
write (FNode f b) v db = b (fnread v db) db

nequal (FNode fa _) (FNode fb _) = FNode (\db -> (fa db) == (fb db)) norev

nsp n = msp $ fnread n db

db = DB { a = 12, b = [2, 3, 4], c = "asdf" }

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
  msp $ write _a 123 $ write _c "zxcv" db
  nsp $ fnoo `nequal` 120
  nsp $ fnoo `nequal` 121
