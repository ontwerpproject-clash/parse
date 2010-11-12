module Main where

import System (getArgs)

-- GHC API
import GHC.Paths ( libdir )

-- VHDL Imports
import Language.VHDL.AST

-- CLasH Imports
import CLasH.Translator(getVHDL)


main = do
  args <- getArgs
  vhdls <- getVHDL libdir args
  elem = parseVhdlAsts vhdls
  putStrLn $ show elem

-- TODO werk de hele lijst netjes af
parseVhdlAsts :: [(VHDLId, DesignFile)] -> ArchElem ()
parseVhdlAsts vhdls
  = parseTopEntity topentity
  where
    types = head vhdls
    topentity = vhdls !! 1

parseTopEntity (VHDLId, DesignFile) -> ArchElem
--TODO met de VHDLId moet misschien nog wat worden gedaan
parseTopEntity (id,df) = parseDesignFile(df)

parseDesignFile :: DesignFile -> ArchElem
-- TODO voor als nog wordt er niet met de ContextItems van de DesignFile gedaan. Hier moet de id uit gehaald worden, voor zover test
parseDesignFile (DesignFile contextItems ls = Function "test" parseLibraryUnits ls

-- TODO mergen van libraries moet nog gebeuren
parseLibraryUnits (l:ls) = first
  where
    res = parseLibraryUnits ls
    first = parseLibraryUnit l
parseLibraryUnits (l:[]) = parseLibraryUnit l

parseLibraryUnit(LUEntity x)      = parseLUEntity x
parseLibraryUnit(LUArch x)        = parseLUArch x
parseLibraryUnit(LUPackageDec x)  = parseLUPackageDec x
parseLibraryUnit(LUPackageBody x) = parseLUPackageBody x

-- VHDL.AST: LUEntity EntityDec
parseLUEntity(x) = parseEntityDec x
-- VHDL.AST: LUArch ArchBody
parseLUArch(x) = parseArchBody x
--TODO
-- parseLuPackageDec en parseLUPackageBody


--parseEntityDec::EntityDec-> Int-> Function
--parseEntityDec (EntityDec id isds) n=Function (parseId id) (Just (parseId id)) ins (turnIdsToOut outs n) ([],[]) ()
--                                     where isdsParsed=map parseEntityDec isds
--                                           ins=getIns isdsParsed
--                                           outs=getOuts isdsParsed

parseEntityDec::EntityDec-> [(VHDLId, (PortID->Port))] -> Function
parseEntityDec (EntityDec id isds) typeTable = Function name (Just name) ins (MultiPort (name ++ "_out") outs) ([],[]) ()
                                                                where name=parseId id
																      isdsParsed=map (parseIfaceSigDec typeTable) isds
                                                                      ins=getIns isdsParsed
                                                                      outs=getOuts isdsParsed

getIns :: [(Port,bool)]-> [Port]
getIns ((x,True):xs)   =x: (getIns xs)
getIns ((x,False):xs)  =getIns xs
getOuts :: [(Port,bool)]-> [Port]
getOuts ((x,True):xs)  =getIns xs
getOuts ((x,False):xs) =x: (getIns xs)


parseIfaceSigDec :: [(VHDLId, (PortID->Port))] -> IfaceSigDec -> (Port,Bool)
parseIfaceSigDec typeTable (IfaceSigDec sigId In t) = (currentMatch (parseId sigId),True) 
                                                       where currentMatch=findInTable typeTable t            
parseIfaceSigDec typeTable (IfaceSigDec sigId Out t)= (currentMatch (parseId sigId),False) 
                                                       where currentMatch=findInTable typeTable t      

findInTable :: [(x,y)] -> x -> y
findInTable  [] t=error $"This type doesn't seem to exist!" ++ show t
findInTable ((x,y):ts) t |(x==t)         =y
                         |otherwise      =findInTable ts t
   
   
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
              
--== Helper functions
--=========================              
parseMode (In)=True
parseMode (Out)=False

getIns::[(Id,bool)]-> [Id]
getIns=filter (x-> mysnd x) 

getOuts::[(Id,bool)]-> [Id]
getOuts=filter (x-> not(mysnd x)) 

turnIdsToOut::[Id]-> Int-> Out
turnIdsToOut [x] n =SinglePort x
turnIdsToOut [] n  =SinglePort "deze wordt nooit gebruikt; niet tekenen."
turnIdsToOut x n =MultiPort (newPortId (n)) (map turnIdToOud x)
                  where turnIdToOud y=Normal y
