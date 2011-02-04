module Main where

import ParseClash

import System (getArgs)

import Ppr

main = do
  args <- getArgs
  elem <- parseClashFiles args
  --putStrLn $ prettyprint elem
  putStrLn ""
