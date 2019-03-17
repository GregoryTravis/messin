module Main where

import Control.Exception (try)
import System.IO (getLine, putStrLn)
import System.IO.Error (isEOFError)

import Util

muts :: IO (Maybe (String, String))
muts = do
  input <- try getLine
  case input of 
    Left e -> if isEOFError e then return Nothing else ioError e
    Right line -> do
      let [cmd, arg] = words line
      return $ Just (cmd, arg)

main :: IO ()
main = do
  x <- muts
  msp x
