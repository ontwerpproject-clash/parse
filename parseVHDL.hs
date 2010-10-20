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

parseEntityDec::EntityDec-> Int-> Function
parseEntityDec (EntityDec id isds) n=Function (parseId id) (Just (parseId id)) ins (turnIdsToOut outs n) ([],[]) ()
                                     where isdsParsed=map parseEntityDec isds
                                           ins=getIns isdsParsed
                                           outs=getOuts isdsParsed

     
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

