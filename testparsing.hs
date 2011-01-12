module Main where

import ParseClash

import System (getArgs)

import Text.PrettyPrint.HughesPJ
import Ppr


main = do
  args <- getArgs
  elem <- parseClashFiles args
  putStrLn $ (renderStyle mystyle . ppr) elem
  where mystyle = style{lineLength=80, ribbonsPerLine=1}
