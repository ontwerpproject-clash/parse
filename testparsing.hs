module Main where

import ParseClash

import System (getArgs)

main = do
  args <- getArgs
  elem <- parseClashFiles args
  putStrLn $ show elem

