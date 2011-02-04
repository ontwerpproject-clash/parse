-- | This module is an external interface to Clash and our parsing of its output.
--   If you want to use our code you should only have to import this module and nothing else.
module ParseClash
(
parseClashFile, parseClashFiles,
ArchElem(..),
Wire(..),
Port(..),
Name,Id,PortId,
In,Out,
InPort,OutPort,
OpType,Value
) where

-- local imports
import Datastruct
import ParseVHDL
import Ppr

-- GHC API
import GHC.Paths ( libdir )

-- CLasH Imports
import CLasH.Translator(getVHDL)

-- | Parse a single clash inputfile into an ArchElem
parseClashFile :: FilePath -> IO (ArchElem ())
parseClashFile x = parseClashFiles [x]

-- | Parse multiple clash inputfiles into a single ArchElem
parseClashFiles :: [FilePath] -> IO (ArchElem ())
parseClashFiles xs
  = do
    vhdls <- getVHDL libdir xs
    let result = parseVhdlAsts vhdls
    putStrLn ""
    putStrLn $ prettyprint result
    return result

