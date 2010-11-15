module Main where

import System (getArgs)
import Data.Maybe (isNothing)

-- GHC API
import GHC.Paths ( libdir )

-- VHDL Imports
import Language.VHDL.AST hiding (Function)

-- CLasH Imports
import CLasH.Translator(getVHDL)

import Datastruct
import ParseTypes

type Types = [(VHDLId, PortId -> Port)]

main = do
  args <- getArgs
  vhdls <- getVHDL libdir args
  let elem = parseVhdlAsts vhdls
    in putStrLn $ show elem

-- TODO werk de hele lijst netjes af
parseVhdlAsts :: [(VHDLId, DesignFile)] -> ArchElem ()
parseVhdlAsts vhdls
  = parseTopEntity topentity types
  where
    typesAst = head vhdls
    topentity = vhdls !! 1
    types = parseTypes typesAst

parseTopEntity :: (VHDLId, DesignFile) -> Types -> ArchElem ()
--TODO met de VHDLId moet misschien nog wat worden gedaan
parseTopEntity (id,df) = parseDesignFile(df)

parseDesignFile :: DesignFile -> Types -> ArchElem ()
-- TODO voor als nog wordt er niet met de ContextItems van de DesignFile gedaan. Hier moet de id uit gehaald worden, voor zover test
parseDesignFile (DesignFile contextItems ls) types = parseEntity ls types

parseEntity ((LUEntity e):ls) types = parseEntityDec e ls types

parseEntityDec (EntityDec id sigs) ls types = result
  where
    result     = Function (parseId id) Nothing ins out (parseArchBody ls) ()
    parsedSigs = filter (\(IfaceSigDec id _ _) -> fromVHDLId id `notElem` ["clock","resetn"]) sigs
    ports = map (parseIfaceSigDec types) parsedSigs
    ins = map (\(p,_) -> p) $ filter snd ports
    out = head $ map (\(p,_) -> p) $ filter (not . snd) ports


parseIfaceSigDec :: Types -> IfaceSigDec -> (Port,Bool)
parseIfaceSigDec typeTable (IfaceSigDec sigId In t)
  | isNothing found = error $ "Could not find type:" ++ show t
  | otherwise = (getPort (parseId sigId),True)
  where
    found = lookup t typeTable
    Just getPort = found
parseIfaceSigDec typeTable (IfaceSigDec sigId Out t)
  | isNothing found = error $ "Could not find type:" ++ show t
  | otherwise = (getPort (parseId sigId),False)
  where
    found = lookup t typeTable
    Just getPort = found





-- VHDL.AST: LUArch ArchBody
parseLUArch x = parseArchBody x


parseArchBody x = ([],[])





-- Hulp functies
-------------------------
parseId::  VHDLId-> Id
parseId s=fromVHDLId s


--- oude troep??



{-

parseArchBody :: parseArchBody -> [ArchElem a] -> [ArchElem a]
parseArchBody (ArchBody "structural" (NSimple x) bs cs ) fs=fs
                                                             where parsedBs=map parseBlockDecItem bs
															       parsedCs=map parseConcSm cs
																   
	
parseConcSm  (CSBSm x)=undefined													   
parseConcSm (CSSASm (s :<==: x)) = (PortReference (parseVHDLName s),result) --geeft een koppeling van het signaal s aan de uitkomst van de expressie in x terug
                                         where result=parseConWFoms x
										 
{-										
parseConcSm  (CSISm x)=undefined	
parseConcSm  (CSPSm x)=undefined	
parseConcSm  (CSGSm x)=undefined	


parseConWforms ([] w Nothing)=parseWform w
parseConWforms (_ _ _)=undefined

parseWform (Unaffected)=undefined
parseWform (Wform [w]) =parseWformElem w
parseWform (Wform []) = error "Wform isn't supposed to have an empty list as argument."
parseWform (Wform ws)=undefined


parseWformElem (WformElem e Nothing)=parseExpr e
parseWformElem (WformElem _ (Just _))=undefined
   
parseBlockDecItem (BDISPB s)=undefined
parseBlockDecItem (BDISD s) =parseSigDec s

parseSigDec :: SigDec -> String

parseSigDec (SigDec id t Nothing) = parseId id
parseSigDec (SigDec id t (just expr))=undefined
   
-- VHDL.AST: LUArch ArchBody
parseLUArch(LUArch x) = parseArchBody x

--EntityDec VHDLId [IfaceSigDec]
--VHDLId kan over worden geslagen want deze wordt nergens voor gebruikt
--parseEntityDec EntityDec id is n m= parseIfaceSigDecs is n m
-}

-- ArchBody VHDLId VHDLName [BlockDecItem] [ConcSm]
parseArchBody id n bdi cs = res_cons
  where
    -- TODO DBSI ook verwerken
    --res_blocks = parseBlockDecItems(dbi)
    res_concs = parseConcSms(cs) n m

--TODO
--parseBlockDecItem

--TODO
--parseBDISD
--TODO ook nog BDISPB parsen
parseConcSms(c:cs) n m = (parseConcSm c n m):(parseConcSms cs n m)
parseConcSms(c:[]) n m = (parseConcSm c n m):[]

parseConcSm(CSBSm c) n m = parseBlockSm c n m
parseConcSm(CSSASm x) n m = parseConSigAssignSm x n m
parseConcSm(CSISm c) n m = parseCompInsSm c n m
parseConcSm(CSPSm c) n m = parseProcSm c n m
parseConcSm(CSGSm c) n m = parseGenerateSm c n m

--TODO er moet ook nog iets met de VHDLName worden gedaan. Belangrijk voor de wires.
parseConSigAssignSm(x :<==: y) n m = parseConWforms y n m
-- nog een soort van tweede iteratie die de x linkt aan de input van een andere operator

--ConWforms [WhenElse] Wform (Maybe When)
--TODO programmeer WhenElse, wordt niet in het voorbeeld gebruikt.
--TODO wanneer het niet NOTHING is.
parseConWforms (whenelse f Nothing) n m = parseWform f n m

--TODO Wform kan ook worden aangeroepen met constructor Unaffected
parseWform f n m = parseWformElems f n m

parseWformElems (f:fs) n m = (parseWformElem f n m):(parseWormElems fs n m)
parseWformElems (f:[]) n m = (parseWformElem f n m):[]

--TODO de maybe kan ook voorkomen, maar niet in het voorbeeld
-- WformElem Expr (Maybe Expr)   
parseWformElem (WformElem f Nothing) n m = parseExpr f n m

-}