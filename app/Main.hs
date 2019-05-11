module Main where

{-
+ Node as wrapped DB -> a
+ DB field accessor node constructors
+ Make it a tuple
+ backwards function
- clean up
- Rename to hide orig stuff and rename node stuff to look orig
- How are errors
-}

import Control.Applicative
import Util 

data Node a = Node a
  deriving (Eq, Show)

nequal :: Eq a => Node a -> Node a -> Node Bool
nequal (Node a) (Node b) = Node $ a == b

nread (Node a) = a

instance Num a => Num (Node a) where
  (+) (Node a) (Node b) = Node $ a + b
  (*) (Node a) (Node b) = Node $ a * b
  abs (Node a) = Node $ abs a
  signum (Node a) = Node $ signum a
  fromInteger a = Node $ fromInteger a
  negate (Node a) = Node $ negate a

instance Num b => Num (a -> b) where
      negate      = fmap negate
      (+)         = liftA2 (+)
      (*)         = liftA2 (*)
      fromInteger = pure . fromInteger
      abs         = fmap abs
      signum      = fmap signum

data DB = DB { a :: Int, b :: [Int], c :: String }
  deriving (Read, Show)

db = DB { a = 12, b = [2, 3, 4], c = "asdf" }

norev = undefined

root db = FNode id rid
  where rid db _ = db

data FNode b = FNode (DB -> b) (b -> DB -> DB)

fshow :: Show b => FNode b -> DB -> String
fshow (FNode fb rfb) db = show $ fb db

fnread (FNode fb rfb) db = fb db

instance Num a => Num (FNode a) where
  (+) (FNode fa _) (FNode fb _) = FNode (\db -> fa db + fb db) norev
  (*) (FNode fa _) (FNode fb _ ) = FNode (\db -> fa db * fb db) norev
  abs (FNode fa _) = FNode (\db -> abs $ fa db) norev
  signum (FNode fa _) = FNode (\db -> signum $ fa db) norev
  fromInteger i = FNode (\db -> fromInteger i) norev
  negate (FNode fa _) = FNode (\db -> negate $ fa db) norev

_a :: FNode Int
_a = FNode (\db -> a db) (\v db -> db { a = v })
_b = FNode (\db -> b db) norev
_c = FNode (\db -> c db) (\v db -> db { c = v })
_bi i = FNode (\db -> b db !! i) norev

write :: FNode a -> a -> DB -> DB
write (FNode f r) v db = r v db

nsp n = msp $ fnread n db

main = do
{-
  msp $ Node True
  msp $ nread $ nequal (Node 10) (Node 20)
  --msp $ nread $ (Node 10) == (Node 20)
-}
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
{-
  let foo :: Node Int
      foo = 10
  msp foo
  msp $ foo == 14
  msp $ foo == 10
  msp $ (sin^2 + cos^2) 123.4
  let voo :: (a -> Int)
      voo = 12
  msp $ nequal (Node 4) (Node 4)
  msp $ nequal (Node 4) (Node 5)
  msp "Ho"
-}

{-
import Prelude hiding ((+), (==), Eq, Ord, Ordering, Show, Read)
import qualified Prelude as Orig ((+), (==), Eq, Ord, Ordering, Show, Read)

import Mut 
import Util 

{-
main = do
  let a = 10
      b = 20
      ya = a Orig.== b
  msp ya
-}

data DB = DB { a :: Int }
  deriving (Orig.Read, Orig.Show)

type History = [DB]

data Node a = Node a
  deriving (Orig.Eq, Orig.Show)

(+) :: Node Int -> Node Int -> Node Int
(+) (Node a) (Node b) = (Node $ a Orig.+ b)

--class Orig.Eq a => Eq a where
class Eq a where
  (==) :: Node a -> Node a -> Node Bool

--instance Orig.Eq a => Eq a where
  --(==) :: a -> b -> Bool
  --(==) a b = a Orig.== b

instance Orig.Eq a => Eq (Node a) where
  (==) (Node a) (Node b) = Node $ a Orig.== b

--class Eq a => Ord a where
  --compare :: Node a -> Node a -> Node Bool

main = do
  --monadly
  s <- readFile "history.db"
  let db = (read s :: History)
  msp db
  msp $ 1 Orig.+ 2
  msp $ (Node 10) + (Node 20)
  let a :: Node Int
      a = Node 10
      b :: Node Int
      b = Node 20
      foo :: Node Bool
      foo = a == b
      --foo = (Node 10) == (Node 20)
      aa :: Int
      aa = 10
      bb :: Int
      bb = 20
      ya :: Bool
      ya = aa Orig.== bb
  --msp foo
  msp ya
-}
