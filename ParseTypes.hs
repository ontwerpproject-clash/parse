module ParseTypes(parseTypes) where

import GHC.Paths ( libdir )
import CLasH.Translator(getVHDL)

import Datastruct
import Language.VHDL.AST hiding (Function)
import Data.Maybe(catMaybes)

--parseVhdlAsts :: [(VHDLId, DesignFile)] -> ArchElem ()
testParseTypes vhdls
  = types
  where
    typesAst = head vhdls
    topentityAst = vhdls !! 1
    types = parseTypes typesAst


parseTypes :: (VHDLId, DesignFile) -> [(VHDLId,(PortId -> Port))]
parseTypes (id,designfile)
  | id /= (Basic "types") =	error $ "parseTypes: got id: " ++ show id ++ ", expecting types"
  | null pkgDecs = error $ "parseType: couldn't a PackageDec"
  | otherwise = parsePackageDec pkgDec
    where
      DesignFile _ ls = designfile
      pkgDecs = filter isPkgDec ls
      LUPackageDec pkgDec = head pkgDecs
      isPkgDec (LUPackageDec _) = True
      isPkgDec _ = False

parsePackageDec :: PackageDec -> [(VHDLId,(PortId -> Port))]
parsePackageDec (PackageDec id pkgDecItems)
  = catMaybes $ map parsePackageDecItem pkgDecItems

parsePackageDecItem :: PackageDecItem -> Maybe (VHDLId,(PortId -> Port))
parsePackageDecItem (PDITD td) = Just $ parseTypeDec td
parsePackageDecItem (PDISD sd) = Just $ parseSubtypeDec sd
parsePackageDecItem (PDISS _) = Nothing -- function declarations don't give us (multi)ports

parseSubtypeDec :: SubtypeDec -> (VHDLId,(PortId -> Port))
parseSubtypeDec (SubtypeDec id (SubtypeIn _ _)) = (id,\portId -> SinglePort portId)

parseTypeDec :: TypeDec -> (VHDLId,(PortId -> Port))
parseTypeDec (TypeDec id def) = (id, parseTypeDef def)

parseTypeDef :: TypeDef -> (PortId -> Port)
parseTypeDef (TDR def) = parseRecordTypeDef def
--TODO: add other nested types
parseTypeDef _ = \portId -> SinglePort portId

parseRecordTypeDef :: RecordTypeDef -> (PortId -> Port)
parseRecordTypeDef (RecordTypeDef elements) = \portId -> MultiPort portId $ contents portId
  where contents portId = map (parseElementDec portId) elements

parseElementDec :: PortId -> ElementDec -> Port
parseElementDec portId (ElementDec eId typemark)
  --TODO add support for tuples inside of tuples: (a,(b,c))
  = SinglePort $ makeNestedPortId portId eId
  where
    makeNestedPortId pId eId = pId ++ '.' : fromVHDLId eId

