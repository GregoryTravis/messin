{-# LANGUAGE OverloadedStrings #-}

module Main where

{-
+ Node as wrapped DB -> a
+ DB field accessor node constructors
+ Make it a tuple
+ backwards function
+ clean up
+ write takes a node instead of a raw value (necessary to not have to pass db everywhere)
- string literal too
- nequal
- Node monad: collect writes, then apply them sequentially
- Get rid of all explicit mentions of db; top level 'nmain' should be inside the node monad and runNode or whatever passes in the db, then saves the resulting
  modified db
- N
- Rename to hide orig stuff and rename node stuff to look orig
- How are errors
-}

import Control.Applicative
import Data.String (IsString(..))
import Util 

data DB = DB { a :: Int, b :: [Int], c :: String }
  deriving (Read, Show)

norev = undefined

--root db = FNode id rid
  --where rid db _ = db

data FNode a = FNode (DB -> a) (a -> DB -> DB)

fshow :: Show a => FNode a -> DB -> String
fshow (FNode f b) db = show $ f db

fnread (FNode f b) db = f db

instance Num a => Num (FNode a) where
  (+) (FNode fa _) (FNode fb _) = FNode (\db -> fa db + fb db) norev
  (*) (FNode fa _) (FNode fb _ ) = FNode (\db -> fa db * fb db) norev
  abs (FNode f _) = FNode (\db -> abs $ f db) norev
  signum (FNode f _) = FNode (\db -> signum $ f db) norev
  fromInteger i = FNode (\db -> fromInteger i) norev
  negate (FNode f _) = FNode (\db -> negate $ f db) norev

instance IsString a => IsString (FNode a) where
  fromString s = FNode (\db -> fromString s) norev

_a :: FNode Int
_a = FNode (\db -> a db) (\v db -> db { a = v })
_b = FNode (\db -> b db) norev
_c = FNode (\db -> c db) (\v db -> db { c = v })
_bi i = FNode (\db -> b db !! i) norev

write :: FNode a -> FNode a -> DB -> DB
write (FNode f b) v db = b (fnread v db) db

nsp n = msp $ fnread n db

db = DB { a = 12, b = [2, 3, 4], c = "asdf" }

main = do
  msp "hi"
  let fnoo :: FNode Int
      fnoo = 121
  nsp fnoo
  nsp _a
  nsp _b
  nsp _c
  nsp $ _bi 1
  msp $ write _a fnoo db
  msp $ write _a 122 db
  msp $ write _c "zxcv" db
  msp $ write _a 123 $ write _c "zxcv" db
