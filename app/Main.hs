module Main where

import Mut 
import Util 

data DB = DB { a :: Int }
  deriving (Read, Show)

type History = [DB]

main = do
  --monadly
  s <- readFile "history.db"
  let db = (read s :: History)
  msp db
