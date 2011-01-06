module ParseState (parseState) where
import Language.VHDL.AST
import Datastruct
import {-# SOURCE #-} ParseVHDL

parseState :: BlockSm -> [(String,Port)] -> EnvSession (ArchElem (), Backtrack)

