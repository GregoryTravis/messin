module Main where

import Util 

data Node a = Node a
  deriving (Eq, Show)

nequal :: Eq a => Node a -> Node a -> Node Bool
nequal (Node a) (Node b) = Node $ a == b

nread (Node a) = a

main = do
  msp $ Node True
  msp $ nread $ nequal (Node 10) (Node 20)
  --msp $ nread $ (Node 10) == (Node 20)
  msp "hi"

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
