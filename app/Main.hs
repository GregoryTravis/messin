module Main where

import Mut 
import Util 

data DB = DB { a :: Int }
  deriving (Read, Show)

main = do
  --monadly
  s <- readFile "history.db"
  let db = (read s :: DB)
  msp db
