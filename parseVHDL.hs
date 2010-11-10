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
--TODO parseTopEntity (id,Designfile) =

parseDesignFile :: DesignFile -> ArchElem
parseDesignFile (DesignFile id (l:ls)) = (Function id (Just id) _ _ _)
  where
    --TODO (res_a,res_w,res_aw,n,m) = parseDesignFile ls
		--TODO (a,w,aw,res_n,res_m) = parseLibraryUnit l
parseDesignFile (DesignFile id (l:[])) = parseLibraryUnit l

--TODO doe de merging van de LUEntity en de LUArch
mergeLibraryUnits x y = undefined

--TODO
--parseLibraryUnit



-- VHDL.AST: LUEntity EntityDec
parseLUEntity(LUEntity x) = parseEntityDec x

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
findInTable  [] t=error "This type doesn't seem to exist!" ++ show t
findInTable ((x,y):ts) t |(x==t)         =y
                         |otherwise      =findInTable ts t
   
   
parseArchBody :: parseArchBody -> [ArchElem a] -> [ArchElem a]
parseArchBody (ArchBody "structural" (NSimple x) bs cs ) fs=fs
                                                             where parsedBs=map parseBlockDecItem bs
															       parsedCs=map parseConcSm cs
																   
	
parseConcSm  (CSBSm x)=undefined													   
parseConcSm (CSSASm (s :<==: x)) = (PortReference (parseVHDLName s),result) --geeft een koppeling van het signaal s aan de uitkomst van de expressie in x terug
                                         where result=parseConWFoms x
										 
										
parseConcSm  (CSISm x)=undefined	
parseConcSm  (CSPSm x)=undefined	
parseConcSm  (CSGSm x)=undefined	


parseConWforms ([] w Nothing)=parseWform w
parseConWforms (_ _ _)=undefined

parseWform (Unaffected)=undefined
parseWform (Wform [w]) =parseWformElem w

parseWformElem (WformElem e Nothing)=parseExpr e
parseWformElem (WformElem _ (Just _))=undefined
   
parseBlockDecItem (BDISPB s)=undefined
parseBlockDecItem (BDISD s) =parseSigDec s

parseSigDec :: SigDec -> String
parseSigDec (SigDec id t Nothing) = parseId id
parseSigDec (SigDec id t (just expr))=undefined
   
-- VHDL.AST: LUArch ArchBody
parseLUArch(LUArch x) = parseArchBody x

--TODO
-- parseLuPackageDeck en parseLUPackageBody
--TODO
--parseArchBody 

--TODO
--parseBlockDecItem

--TODO
--parseBDISD
--TODO ook nog BDISPB parsen

--TODO: CSBSm BlockSm	& CSISm CompInsSm	 & CSPSm ProcSm	 & CSGSm GenerateSm 
-- VHDL.AST: CSSASm ConSigAssignSm	
parseConcSm(CSSASm x) = parseConSigAssignSm x

parseConSigAssignSm(x :<==: y) n m = (a, w, as, b, c)
						where
							(a, w, as, b, c) = parseConWforms y n m
							-- nog een soort van tweede iteratie die de x linkt aan de input van een andere operator


              
              
              
parseMode (In)=True
parseMode (Out)=False

getIns::[(Id,bool)]-> [Id]
getIns=filter (x-> mysnd x) 

getOuts::[(Id,bool)]-> [Id]
getOuts=filter (x-> not(mysnd x)) 

turnIdsToOut::[Id]-> Int-> Out
turnIdsToOut [x] n =Normal x
turnIdsToOut [] n  =Normal "deze wordt nooit gebruikt; niet tekenen."
turnIdsToOut x n =Tuple (newPortId (n)) (map turnIdToOud x)
                  where turnIdToOud y=Normal y

parseEntityDec (IfaceSigDec id m type)=((parseId id),parseMode m)  ||met het type van het signaal wordt nog niets gedaan

