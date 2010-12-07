
module ParseVHDL where

import Data.Maybe (isNothing,catMaybes)
import Data.List

-- VHDL Imports
import Language.VHDL.AST hiding (Function)

import Helper
import Datastruct
import ParseTypes
import ParseExpr hiding (parseVHDLName)

type Types = [(VHDLId, PortId -> Port)]

-- TODO werk de hele lijst netjes af
parseVhdlAsts :: [(VHDLId, DesignFile)] -> ArchElem ()
parseVhdlAsts vhdls
  = parseTopEntity topentity types
  where
    typesAst = head vhdls
    topentity = vhdls !! 1 --TODO: parse de overige enteties
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
    (LUArch archbody):_ = filter isLUArch ls  --TODO: doe iets met de overige archbody's als die bestaan
    result = parseArchBody archbody [legefunctie] --voeg hier types toe zodat deze gebruikt kan worden bij het maken van de multiports
    parsedSigs = filter (\(IfaceSigDec id _ _) -> fromVHDLId id `notElem` ["clock","resetn"]) sigs
    ports = map (parseIfaceSigDec types) parsedSigs
    ins = map fst $ filter snd ports
    out = head $ map fst $ filter (not . snd) ports
    isLUArch (LUArch _) = True
    isLuArch _ = False

parseIfaceSigDec :: Types -> IfaceSigDec -> (Port,Bool)
parseIfaceSigDec typeTable (IfaceSigDec sigId direction t)
  | isNothing found = error $ "Could not find type:" ++ show t
  | otherwise = (getPort (parseId sigId),isIn)
  where
    found = lookup t typeTable
    Just getPort = found
	isIn = direction==In 

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
removeReferences (ws,(a@(PortReference (SinglePort x)):as)) table ins
  | x `elem` ins = (fst niksVeranderd, snd niksVeranderd) --HACKED??
  {-| otherwise = (union (fst r) (ws \\ [w]), snd r)   --dit hoeft niet? -}
  | otherwise = (fst r , snd r)  --veranderd !!!!!!!!!!!!!!!!!!!!!!! -}
    where
      r=  removeReferences ((ws \\ [w])  ++ (fst newReferences),(snd newReferences) ++ as ) table ins
      newReferences=resolveAssociationNamed table ins i x --mogelijk moeten alle associaties eerder worden verholpen om i te kunnen vinden, dan krijgen we de error in findInof..
      (i,w)= findInof x ws  --w dient nu verwijderd te worden (het nesten van signalen wordt nl niet toegestaan)
      niksVeranderd=  removeReferences (ws,as) table ins
      --Just res = lookup a table
      --i = getHighest $ outportOf$ fst5 res

removeReferences (ws,(PortReference (MultiPort _ _)):as) table ins = undefined --zal dit ooit voorkomen?
                                                                      where  r=  removeReferences (ws,as)
removeReferences (ws,(a:as)) table ins= (fst r, a: snd r)
                              where  r=  removeReferences (ws,as) table ins
removeReferences (ws,[]) table ins = (ws,[])

findInof :: PortId -> [Wire a] -> (PortId,Wire a)
findInof p []=error $ "kan " ++ show p ++ " niet vinden, blijkbaar moeten alle wires eerst worden gevonden aangezien er nu 1 mist.."
findInof p ((w@(Wire name x y _)):ws)
  |name /= Nothing && n == x = (y,w)
  |x==p       = (y,w)
  |otherwise = findInof p ws
  where
    Just n = name
resolveassociation ::  [(ArchElem (),(ArchElem (),[Wire ()],[ArchElem ()],Int,Int))] -> [String] -> String -> ([Wire ()],[ArchElem ()])
resolveassociation table ins i =resolveAssociationNamed table ins i i

resolveAssociationNamed ::  [(ArchElem (),(ArchElem (),[Wire ()],[ArchElem ()],Int,Int))] -> [String] -> String -> String -> ([Wire ()],[ArchElem ()])
resolveAssociationNamed table ins outName x
--  | lkup == Nothing = error $ "We kunnen " ++ i ++ " niet vinden in :\n" ++ (unlines $ map show table)
  | otherwise =followUp
  where
    deze = (PortReference $ SinglePort x)
    lkup =lookup deze table
    Just currRes = lkup
    (firstElem,_,_,_,_)=currRes
    PortReference (SinglePort firstElemStr) = firstElem -- TODO : kan dit ook multiport zijn?
    isIn = (isInSignal deze ins)
    doorgaan=checkIsReference firstElem && ( not (isInSignal firstElem ins))
    followedUp=resolveAssociationNamed table ins outName firstElemStr
    followUp |isIn = ([],[])
             |doorgaan =((fst followedUp) ++ (get2out5 currRes) , (snd followedUp) ++ (get3out5 currRes))
             |otherwise=((Wire (Just x) (getHighest(outportOf firstElem)) outName () ) : (get2out5 currRes),(get1out5 currRes : get3out5 currRes))
			 {-|otherwise=( (Wire (Just x) (getHighest(outportOf firstElem)) outName () ) : (get2out5 currRes),(get1out5 currRes : get3out5 currRes))-}

isInSignal (PortReference (SinglePort x)) ins=elem x ins
isInSignal (PortReference (MultiPort _ _)) ins=undefined --kan nu nog niet voorkomen in prototype..

checkIsReference:: ArchElem a -> Bool
checkIsReference (PortReference z)=True
checkIsReference _=False

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

----------------------------------------------------------------------------------------------------

parseConcSm :: ConcSm -> Int -> Int -> (ArchElem (),(ArchElem (),[Wire ()],[ArchElem ()],Int,Int))
parseConcSm  (CSBSm x) n m =undefined
parseConcSm (CSSASm (s :<==: x)) n m
  = (PortReference $ SinglePort (parseVHDLName s),(head alleElementen,get2out5 $ last result,tail alleElementen,get4out5 $ last result,get5out5 $ last result)) --geeft een koppeling van het signaal s aan de uitkomst van de expressie in x terug
    where
      result :: [(ArchElem (),[Wire ()],[ArchElem ()],Int,Int)]
      result=parseConWforms x n m
      alleElementen = concat $ map (\(a,_,as,_,_) -> a:as) result
parseConcSm  (CSISm x) n m =undefined
parseConcSm  (CSPSm x) n m=undefined
parseConcSm  (CSGSm x) n m=undefined


--TODO ook nog BDISPB parsen
parseConcSms [] n m = []
parseConcSms(c:cs) n m = (parseConcSm c n m):(parseConcSms cs n m)

--Deze kan verwijderd worden.
--TODO er moet ook nog iets met de VHDLName worden gedaan. Belangrijk voor de wires.
--parseConSigAssignSm(x :<==: y) n m = parseConWforms y n m

parseConWforms (ConWforms [] f Nothing) n m = parseWform f n m
parseConWforms (ConWforms x f Nothing) n m
   |length selects /= 0  = [trueResult]
   |otherwise           = error  "geen whenElses, dat klopt niet?"
     where
           currMux=Mux (operatorId m) inportNames (SinglePort (newPortId n)) selectNames ()
           inportNames=[newPortId (number+n) |number<- [1..totalIns]]
           totalIns=(length x+1)
           secondN=n+totalIns
           totalSelects=length x
           selectNames=[newPortId (number+secondN) |number<- [1..totalSelects]]
           thirdN=secondN+totalSelects+1

           parsedWhenElses= parseWhenElses x thirdN (m+1)
           otherwiseUitgang=parseWform f newN newM
           newN=fst (fst parsedWhenElses)
           newM=snd (fst parsedWhenElses)
           (ins,selects)=unzip (snd parsedWhenElses) --selects moet nog gekoppeld worden


           tempResult=connect ((concat ins) ++ otherwiseUitgang) currMux "a mux input wire" --select ingang moet nog gekoppeld worden, is last hier zo goed?
           trueResult=connectSelects selects tempResult "a select mux wire"






parseWhenElses::  [WhenElse] -> Int -> Int -> ((Int,Int),
                                              [([(ArchElem (),[Wire ()],[ArchElem ()],Int,Int)]
                                              ,(ArchElem (),[Wire ()],[ArchElem ()],Int,Int))])
parseWhenElses xs n m =mapAccumL parseWhenElse (n,m) xs

parseWhenElse::  (Int,Int) -> WhenElse -> ((Int,Int),
                                         ([(ArchElem (),[Wire ()],[ArchElem ()],Int,Int)]
                                         ,(ArchElem (),[Wire ()],[ArchElem ()],Int,Int)))
parseWhenElse (n,m) (WhenElse wform expr)
   =( nm2tuple,(resultWform,resultGaurd))
     where resultWform=parseWform wform n m
           resultGaurd=parseExpr expr (get4out5 (last resultWform)) (get5out5 (last resultWform)) --volgens mij is resultWform altijd maar 1 element
           nm2tuple=(get4out5 resultGaurd, get5out5 resultGaurd)


--verbind de meegegeven geparse delen aan het meegegeven architectuurElement.
connect:: [(ArchElem (),[Wire ()],[ArchElem ()],Int,Int)] -> ArchElem () -> String -> (ArchElem (),[Wire ()],[ArchElem ()],Int,Int)
connect xs (m@(Mux _ inportNames _ _ _)) name
     =(m,allWires,allElems,finalN,finalM)
       where
        newWires=map makeNewWire (zip (map (getHighest.outportOf.get1out5) xs) inportNames)
        allElems=(map get1out5 xs) ++ concat (map get3out5 xs)
        allWires=newWires ++ concat (map get2out5 xs)
        finalN= get4out5 (last xs)
        finalM= get5out5 (last xs)
        makeNewWire (x,i)=Wire (Just name) x i ()



--lijkst heel sterk op connect, misschien 1 algemenere functie maken die beide afhandeld?
connectSelects xs ((mux@(Mux _ _ _ selectNames _)),wires,elems,n,m) name
     =(mux,allWires,allElems,finalN,finalM)
       where
        newWires=map makeNewWire (zip (map (getHighest.outportOf.get1out5) xs) selectNames)
        allElems=(map get1out5 xs) ++ concat (map get3out5 xs) ++ elems
        allWires=newWires ++ concat (map get2out5 xs) ++ wires
        finalN= get4out5 (last xs)
        finalM= get5out5 (last xs)
        makeNewWire (x,i)=Wire (Just name) x i ()



--TODO Wform kan ook worden aangeroepen met constructor Unaffected
parseWform (Wform f) n m = parseWformElems f n m

parseWformElems [] n m = []
parseWformElems (f:fs) n m = (parseWformElem f n m):(parseWformElems fs n m)

--TODO de maybe kan ook voorkomen, maar niet in het voorbeeld
parseWformElem (WformElem f Nothing) n m = parseExpr f n m

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
