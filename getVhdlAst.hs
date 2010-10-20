{-
Om deze file te kunnen gebruiken moet je onze eigen speciale clash versie installeren.
deze is te vinden in http://github.com/ontwerpproject-clash/clash
  Gewoon clonen(of downloaden),
  naar de clash directory gaan
  en installeren met "cabal install"


om een vhdl ast te zien kun je het volgende doen

open deze file in ghci met 
ghci getVhdlAst.hs

vertaal macc.hs naar vhdl en stop de vhdl ast in de lijst a

a <- getVHDL libdir ["macc.hs"]

a is nu een lijst met paren van een naam en een vhdl ast, 1 voor elke file die clash zou outputten(maar sommige zijn dubbel)

met 
map fst a
kun je kijken welke file je allemaal hebt

en met 
a !! 1
kun bijvoorbeeld de 2e file als ast bekijken

Maar via ghci lijkt niet te werken op windows.
Op windows moet je deze file daarom maar compileren en runnen met als argument een file die je wil vertalen.
-}

module Main where

import System (getArgs)

-- GHC API
import GHC.Paths ( libdir )

-- VHDL Imports
import qualified Language.VHDL.AST as AST
--import qualified Language.VHDL.FileIO as FileIO
--import qualified Language.VHDL.Ppr as Ppr

-- CLasH Imports
import CLasH.Translator(getVHDL)


main = do
  args <- getArgs
  vhdls <- getVHDL libdir args
  putStrLn "Top level entity"
  putStrLn $ show $ vhdls !! 1
