module Main where

{-
+ Node as wrapped DB -> a
+ DB field accessor node constructors
+ Make it a tuple
+ backwards function
- clean up
- nequal
- write takes a node instead of a raw value
- N
- Rename to hide orig stuff and rename node stuff to look orig
- How are errors
-}

import Control.Applicative
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

_a :: FNode Int
_a = FNode (\db -> a db) (\v db -> db { a = v })
_b = FNode (\db -> b db) norev
_c = FNode (\db -> c db) (\v db -> db { c = v })
_bi i = FNode (\db -> b db !! i) norev

write :: FNode a -> a -> DB -> DB
write (FNode f b) v db = b v db

nsp n = msp $ fnread n db

db = DB { a = 12, b = [2, 3, 4], c = "asdf" }

main = do
  msp "hi"
  let fnoo :: FNode Int
      fnoo = 10
  nsp fnoo
  nsp _a
  nsp _b
  nsp _c
  nsp $ _bi 1
  msp $ write _a 120 db
  msp $ write _c "zxcv" db
  msp $ write _a 120 $ write _c "zxcv" db
