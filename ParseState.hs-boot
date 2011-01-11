module ParseState (parseState) where
import Language.VHDL.AST
import Datastruct
import Helper

parseState :: BlockSm -> [(String,Port)] -> EnvSession (ArchElem (), Backtrack)