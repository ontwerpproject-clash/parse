module ParseState (parseState) where
import Language.VHDL.AST
import Datastruct

parseState :: BlockSm -> [(String,Port)] -> Int -> Int -> (ArchElem (),(ArchElem (),[Wire ()],[ArchElem ()],Int,Int))

