module Main where

-- Node as wrapped DB -> a
-- Rename to hide orig stuff and rename node stuff to look orig

import Control.Applicative
import Util 

data Node a = Node a
  deriving (Eq, Show)

nequal :: Eq a => Node a -> Node a -> Node Bool
nequal (Node a) (Node b) = Node $ a == b

nread (Node a) = a

data DB = DB { a :: Int, b :: [Int] }
  deriving (Read, Show)

db = DB { a = 12, b = [2, 3, 4] }

data FNode b = FNode (DB -> b)

fshow :: Show b => FNode b -> DB -> String
fshow (FNode fb) db = show $ fb db

fnread (FNode fb) db = fb db

instance Num a => Num (FNode a) where
  (+) (FNode fa) (FNode fb) = FNode $ \db -> fa db + fb db
  (*) (FNode fa) (FNode fb) = FNode $ \db -> fa db * fb db
  abs (FNode fa) = FNode $ \db -> abs $ fa db
  signum (FNode fa) = FNode $ \db -> signum $ fa db
  fromInteger i = FNode $ \db -> fromInteger i
  negate (FNode fa) = FNode $ \db -> negate $ fa db

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

main = do
  msp $ Node True
  msp $ nread $ nequal (Node 10) (Node 20)
  --msp $ nread $ (Node 10) == (Node 20)
  msp "hi"
  let fnoo :: FNode Int
      fnoo = 10
  msp $ fnread fnoo db
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
