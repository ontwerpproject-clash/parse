module Main where

import System (getArgs)
import Data.Maybe (isNothing)
import Data.Maybe(catMaybes)
import Data.List

-- GHC API
import GHC.Paths ( libdir )

-- VHDL Imports
import Language.VHDL.AST hiding (Function)

-- CLasH Imports
import CLasH.Translator(getVHDL)

import Datastruct
import ParseTypes
import ParseExpr hiding (parseVHDLName)

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
    legefunctie     = Function (parseId id) Nothing ins out ([],[]) ()
    (LUArch archbody):_ = filter isLUArch ls
    result = parseArchBody archbody [legefunctie]
    parsedSigs = filter (\(IfaceSigDec id _ _) -> fromVHDLId id `notElem` ["clock","resetn"]) sigs
    ports = map (parseIfaceSigDec types) parsedSigs
    ins = map (\(p,_) -> p) $ filter snd ports
    out = head $ map (\(p,_) -> p) $ filter (not . snd) ports
    isLUArch (LUArch _) = True
    isLuArch _ = False


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



-- Hulp functies
-------------------------


--- oude troep??



{-

=======
parseLUArch(x) = parseArchBody x
--TODO
-- parseLuPackageDec en parseLUPackageBody


--parseEntityDec::EntityDec-> Int-> Function
--parseEntityDec (EntityDec id isds) n=Function (parseId id) (Just (parseId id)) ins (turnIdsToOut outs n) ([],[]) ()
--                                     where isdsParsed=map parseEntityDec isds
--                                           ins=getIns isdsParsed
--                                           outs=getOuts isdsParsed



-}
-----------------------------------------parseArchBody----------------------------------------------   
parseArchBody :: ArchBody -> [ArchElem ()] -> ArchElem ()
parseArchBody (ArchBody (Basic "structural") (NSimple x) bs cs ) fs
   =newElem -- : (delete currentArchElem fs)
    where
      parsedBs=catMaybes $ map parseSignalDecsOf bs
      -- parsedCs=mapWpassedInts parseConcSm 0 0 cs --mapWpassedInts is soortgelijk aan mapAccumL. Dit later dus met mapAccumL doen.
      (_,parsedCs)=mapAccumL (myParseConcSm) (0,0) cs
      myParseConcSm :: (Int,Int) -> ConcSm -> ((Int,Int), (ArchElem (),(ArchElem (),[Wire ()],[ArchElem ()],Int,Int)))
      myParseConcSm (n,m) c = ((n',m'), result)
        where
          result = parseConcSm c n m
          (_,(_, _, _, n', m')) = result
      currentArchElem=searchFunction (parseId x) fs
      outsResolved  :: [([Wire ()],[ArchElem ()])]
      outsResolved=map (resolveassociation parsedCs (inSignalsOf currentArchElem))  (outSignalsOf currentArchElem)
      concatted :: ([Wire ()],[ArchElem ()])
      concatted= (concat $ fst unzipped, concat $ snd unzipped)
      unzipped = unzip outsResolved
      internals=removeReferences concatted parsedCs (inSignalsOf currentArchElem)
      newElem= addInternals currentArchElem internals
parseArchBody (ArchBody _ (NSimple x) bs cs ) fs=undefined
parseArchBody (ArchBody (Basic "structural") _ bs cs ) fs=undefined
parseArchBody (ArchBody _ _ bs cs ) fs=undefined

addInternals :: ArchElem a -> ([Wire a],[ArchElem a]) -> ArchElem a
addInternals (Function q w e r _ a) (ws,as)=Function q w e r (as,ws) a
addInternals _ _=error "can not add internals to an architecture element that is not a Function"


removeReferences :: ([Wire ()],[ArchElem ()]) -> [(ArchElem (),(ArchElem (),[Wire ()],[ArchElem ()],Int,Int))] -> [String] -> ([Wire ()],[ArchElem ()])
removeReferences (ws,(PortReference (SinglePort x):as)) table ins
 = ((fst r) ++ ws, snd r)
  where r=  removeReferences ((ws \\ [w])  ++ (fst newReferences),(snd newReferences) ++ as ) table ins
        newReferences=resolveAssociationNamed table ins i x --mogelijk moeten alle associaties eerder worden verholpen om i te kunnen vinden, dan krijgen we de error in findInof..
        (i,w)= findInof x ws  --w dient nu verwijderd te worden (het nesten van signalen wordt nl niet toegestaan)
removeReferences (ws,(PortReference (MultiPort _ _)):as) table ins = undefined --zal dit ooit voorkomen?
                                                                      where  r=  removeReferences (ws,as)
removeReferences (ws,(a:as)) table ins= (fst r, a: snd r)
                              where  r=  removeReferences (ws,as) table ins

findInof :: PortId -> [Wire a] -> (PortId,Wire a)
findInof p []=error "blijkbaar moeten alle wires eerst worden gevonden aangezien er nu 1 mist.."
findInof p ((w@(Wire _ x y _)):ws) |x==p       = (y,w)
                                   |otherwise = findInof p ws

resolveassociation ::  [(ArchElem (),(ArchElem (),[Wire ()],[ArchElem ()],Int,Int))] -> [String] -> String -> ([Wire ()],[ArchElem ()])
resolveassociation table ins i =resolveAssociationNamed table ins i i

resolveAssociationNamed ::  [(ArchElem (),(ArchElem (),[Wire ()],[ArchElem ()],Int,Int))] -> [String] -> String -> String -> ([Wire ()],[ArchElem ()])
resolveAssociationNamed table ins outName i
  =followUp
  where Just currRes=lookup (PortReference $ SinglePort i) table
        (firstElem,_,_,_,_)=currRes
        PortReference (SinglePort firstElemStr) = firstElem -- TODO : kan dit ook multiport zijn?
        doorgaan=checkIsReference firstElem && ( not (isInSignal firstElem ins))
        followedUp=resolveassociation table ins firstElemStr
        followUp |doorgaan =((fst followedUp) ++ (snd5 currRes) , (snd followedUp) ++ (trd5 currRes))
                 |otherwise=( (Wire (Just i) (getHighest(outportOf firstElem)) outName () ) : (snd5 currRes),(trd5 currRes))


isInSignal (PortReference (SinglePort x)) ins=elem x ins
isInSignal (PortReference (MultiPort _ _)) ins=undefined --kan nu nog niet voorkomen in prototype..

checkIsReference:: ArchElem a -> Bool
checkIsReference (PortReference z)=True
checkIsReference _=False

outportOf :: ArchElem a -> Port
outportOf (Function _ _ _ p _ _) = p
outportOf (Operator _ _ _ p _) = p
outportOf (Literal _ _ p _) = p
outportOf (Mux _ _ p _ _) = p
outportOf (Register _ _ p _) = p
outportOf (PortReference p) = p


inSignalsOf:: ArchElem a -> [String]
inSignalsOf (Function _ _ ins _ _ _)=parseToSingles ins
outSignalsOf:: ArchElem a -> [String]
outSignalsOf (Function _ _ _ out _ _)=parseToSingles [out]

getHighest::Port -> PortId
getHighest (SinglePort x)=x
getHighest (MultiPort x _)=x

parseToSingles::[Port]-> [String]
parseToSingles []=[]
parseToSingles ((SinglePort x):xs) =x:parseToSingles xs
parseToSingles ((MultiPort y ys):xs)= (parseToSingles ys) ++ (parseToSingles xs)

searchFunction:: String -> [ArchElem a] -> ArchElem a
searchFunction s (f@(Function x _ _ _ _ _):fs)
  |x==s        =f
  |otherwise   = searchFunction s fs
searchFunction s []=error $ "a functie with the name" ++ s ++ "was not found by the parser. We might have messed up, sorry for the inconvienience.."
searchFunction s _=error  "the impossible happened" --De architectuurelementen in de invoer horen nl allemaal Funtions te zijn...

parseSignalDecsOf :: BlockDecItem -> Maybe String
parseSignalDecsOf (BDISPB s)=Nothing --wordt later ergens anders geparsed
parseSignalDecsOf (BDISD s) =Just $ parseSigDec s

parseSigDec :: SigDec -> String
parseSigDec (SigDec id t Nothing) = parseId id
parseSigDec x@(SigDec id t (Just expr))="het volgende kan nog niet geparsed worden: " ++ (show x)
{-
mapWpassedInts :: (a -> Int -> Int -> (b,c,d)) -> Int -> Int -> [a] -> [b]
mapWpassedInts  _ _ _ [] = []
mapWpassedInts  f n m (x:xs)=first3 res: (mapWpassedInts f newN newM xs)
                             where res= f x n m
                                   newN=second3 res
                                   newM=third3 res
-}
first3 (x,y,z)=x
second3 (x,y,z)=y
third3 (x,y,z)=z

----------------------------------------------------------------------------------------------------


parseConcSm :: ConcSm -> Int -> Int -> (ArchElem (),(ArchElem (),[Wire ()],[ArchElem ()],Int,Int))
parseConcSm  (CSBSm x) n m =undefined
parseConcSm (CSSASm (s :<==: x)) n m
  = (PortReference $ SinglePort (parseVHDLName s),(head alleElementen,[],tail alleElementen,getN $ last result,getM $ last result)) --geeft een koppeling van het signaal s aan de uitkomst van de expressie in x terug
    where
      result :: [(ArchElem (),[Wire ()],[ArchElem ()],Int,Int)]
      result=parseConWforms x n m
      alleElementen = concat $ map (\(a,_,as,_,_) -> a:[] {-as-}) result
parseConcSm  (CSISm x) n m =undefined
parseConcSm  (CSPSm x) n m=undefined
parseConcSm  (CSGSm x) n m=undefined

{-


parseConWforms ([] w Nothing) n m=parseWform w n m
parseConWforms (_ _ _) n m=undefined

parseWform (Unaffected) n m=undefined
parseWform (Wform [w]) n m=parseWformElem w
parseWform (Wform []) n m= error "Wform isn't supposed to have an empty list as argument."
parseWform (Wform ws) n m=undefined


parseWformElem (WformElem e Nothing) n m=(result,getN result,getM result) --later parseExpr aanpassen om het echte resultaat een subtuple te maken los van n en m...
                                         where result=parseExpr e n m
parseWformElem (WformElem _ (Just _)) n m=undefined
   
   
-- VHDL.AST: LUArch ArchBody
parseLUArch(LUArch x) = parseArchBody x

--EntityDec VHDLId [IfaceSigDec]
--VHDLId kan over worden geslagen want deze wordt nergens voor gebruikt
--parseEntityDec EntityDec id is n m= parseIfaceSigDecs is n m
-}


--TODO
--parseBlockDecItem

--TODO
--parseBDISD
--TODO ook nog BDISPB parsen
parseConcSms(c:cs) n m = (parseConcSm c n m):(parseConcSms cs n m)
parseConcSms(c:[]) n m = (parseConcSm c n m):[]

{-
parseConcSm(CSBSm c) n m = parseBlockSm c n m
parseConcSm(CSSASm x) n m = parseConSigAssignSm x n m
parseConcSm(CSISm c) n m = parseCompInsSm c n m
parseConcSm(CSPSm c) n m = parseProcSm c n m
parseConcSm(CSGSm c) n m = parseGenerateSm c n m
-}
--TODO er moet ook nog iets met de VHDLName worden gedaan. Belangrijk voor de wires.
parseConSigAssignSm(x :<==: y) n m = parseConWforms y n m
-- nog een soort van tweede iteratie die de x linkt aan de input van een andere operator

--ConWforms [WhenElse] Wform (Maybe When)
--TODO programmeer WhenElse, wordt niet in het voorbeeld gebruikt.
--TODO wanneer het niet NOTHING is.
parseConWforms (ConWforms _ f _) n m = parseWform f n m

--TODO Wform kan ook worden aangeroepen met constructor Unaffected
parseWform (Wform f) n m = parseWformElems f n m

parseWformElems (f:fs) n m = (parseWformElem f n m):(parseWformElems fs n m)
parseWformElems (f:[]) n m = (parseWformElem f n m):[]

--TODO de maybe kan ook voorkomen, maar niet in het voorbeeld
-- WformElem Expr (Maybe Expr)   
parseWformElem (WformElem f Nothing) n m = parseExpr f n m


-- gekopieerd uit parseDatastruc.hs


parseVHDLName (NSimple s)=parseSimpleName s
parseVHDLName  (NSelected s)=parseSelectedName s
parseVHDLName  (NIndexed s)=parseIndexedName s
parseVHDLName  (NSlice s)=parseSliceName s
parseVHDLName  (NAttribute s)=parseAttibName s

parseSimpleName ::SimpleName-> Id
parseSimpleName  s=parseId s

parseSelectedName::SelectedName-> Id
parseSelectedName (x :.: y)=(parsePrefix x) ++ (parseSuffix y)

parsePrefix x=parseVHDLName x

parseSuffix:: Suffix -> Id
parseSuffix (SSimple s)=parseSimpleName s
parseSuffix (All)=""

parseIndexedName (IndexedName x es)=undefined
parseSliceName s=undefined
parseAttibName s=undefined

