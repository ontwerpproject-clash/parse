{-# LANGUAGE TemplateHaskell #-}
module ParseVHDL where

import Data.Maybe (isNothing,catMaybes)
import Data.List


-- VHDL Imports
import Language.VHDL.AST hiding (Function)

import Datastruct
import ParseTypes
import {-# SOURCE #-} ParseState

-- These imports are required for easy state sessions
import qualified Control.Monad.Trans.State as State            -- Needs package: transformers
import qualified Data.Accessor.Monad.Trans.State as MonadState -- Needs package: data-accessor-transformers, data-accessor
import qualified Data.Accessor.Template                        -- Needs package: data-accessors-template, data-accessor
import qualified Data.Set as Set

type Types = [(VHDLId, PortId -> Port)]

-- Make a datatype that represents the state
data EnvState = ES {
   counter_   :: Int,
   vhdlFiles_ :: [(VHDLId, DesignFile)],
   types_     :: Types
}
initState = ES 0 [] []

-- Let template-haskell derive easy accessors for the state
Data.Accessor.Template.deriveAccessors ''EnvState

-- Make a type alias for computations that work with the state made above
type EnvSession a = State.State EnvState a

data Backtrack =
  Backtrack {
    archElem      :: ArchElem (),
    wires         :: [Wire ()],
    prevArchElems :: [ArchElem ()]
  } deriving (Eq, Show)

-- Adds one to the counter and return the new Id
getNewId ::  EnvSession Int
getNewId = do
  curCounter <- MonadState.get counter
  let newId = curCounter + 1
  MonadState.set counter newId
  return newId

parseVhdlAsts :: [(VHDLId, DesignFile)] -> ArchElem ()
parseVhdlAsts vhdls
  = fst (runSession (initEnvironment vhdls types) initState)
  where
    typesAst = head vhdls
    types = parseTypes typesAst

-- Example of a pure computation that executes a statefull computation given an initial state
runSession :: EnvSession a -> EnvState -> (a, EnvState)
runSession session initState = State.runState session initState

initEnvironment :: [(VHDLId, DesignFile)] -> Types -> EnvSession (ArchElem ())
initEnvironment vhdls settypes = do
    MonadState.set counter 0
    MonadState.set vhdlFiles vhdls
    MonadState.set types settypes
    let topentity = vhdls !! 1
    parseTopEntity topentity


parseTopEntity :: (VHDLId, DesignFile) -> EnvSession (ArchElem ())
--TODO met de VHDLId moet misschien nog wat worden gedaan
parseTopEntity (id,df) = parseDesignFile(df)

parseDesignFile :: DesignFile -> EnvSession (ArchElem ())
-- TODO voor als nog wordt er niet met de ContextItems van de DesignFile gedaan. Hier moet de id uit gehaald worden, voor zover test
parseDesignFile (DesignFile contextItems ls) = parseEntity ls

parseEntity :: [LibraryUnit] -> EnvSession (ArchElem ())
parseEntity ((LUEntity e):ls) = parseEntityDec e ls 

parseEntityDec :: EntityDec -> [LibraryUnit] -> EnvSession (ArchElem ())
parseEntityDec (EntityDec id sigs) ls = do
    let isLUArch (LUArch _) = True
        isLuArch _          = False
        (LUArch archbody):_ = filter isLUArch ls  --TODO: doe iets met de overige archbody's als die bestaan
        parsedSigs = filter (\(IfaceSigDec id _ _) -> fromVHDLId id `notElem` ["clock","resetn"]) sigs
    ports <- mapM (parseIfaceSigDec) parsedSigs
    let ins = map fst $ filter snd ports
    let out = head $ map fst $ filter (not . snd) ports
    portTable <- mapM (parseIfaceSigDec2) parsedSigs
    let legefunctie = Function (parseId id) Nothing ins out ([],[]) ()
    result <- parseArchBody archbody portTable [legefunctie]
    return result

parseIfaceSigDec :: IfaceSigDec -> EnvSession (Port,Bool)
parseIfaceSigDec (IfaceSigDec sigId direction t) = do
    typeTable <- MonadState.get types
    let found = lookup t typeTable
        Just getPort = found
        isIn = (direction==In )
        result 
          | isNothing found = error $ "Could not find type:" ++ show t
          | otherwise = (getPort (parseId sigId),isIn)
    return result

parseIfaceSigDec2 :: IfaceSigDec -> EnvSession (String, Port)   
parseIfaceSigDec2 (IfaceSigDec id _ t) = do
    typeTable <- MonadState.get types
    let found = lookup t typeTable
        Just getPort = found
        result
          | isNothing found = error $ "Could not find type:" ++ show t
          | otherwise = ((parseId id), getPort (parseId id))
    return result
     
-----------------------------------------parseArchBody----------------------------------------------
parseArchBody :: ArchBody -> [(String,Port)] -> [ArchElem ()] -> EnvSession (ArchElem ())
parseArchBody (ArchBody (Basic "structural") (NSimple x) bs cs ) portTable fs = do
  types <- MonadState.get types
  --tabel met poorten bijbehorend aan de signalen.
  signalDecs <- mapM (parseSignalDecsOf) bs
  let pTable=portTable ++ catMaybes signalDecs 
      myParseConcSm :: ConcSm -> EnvSession (ArchElem (), Backtrack )
      myParseConcSm c = parseConcSm c pTable
  parsedCs <- mapM (myParseConcSm) cs
  let currentArchElem=searchFunction (parseId x) fs
      outsResolved  :: [([Wire ()],[ArchElem ()])]
      outsResolved=map (resolveassociation parsedCs (inSignalsOf currentArchElem))  (outSignalsOf currentArchElem)
      concatted :: ([Wire ()],[ArchElem ()])
      concatted= (concat $ fst unzipped, concat $ snd unzipped)
      unzipped = unzip outsResolved
      internals=removeReferences concatted parsedCs (inSignalsOf currentArchElem)
      newElem= addInternals currentArchElem internals
  return newElem
parseArchBody (ArchBody _ (NSimple x) bs cs ) _ fs=undefined
parseArchBody (ArchBody (Basic "structural") _ bs cs ) _ fs=undefined
parseArchBody (ArchBody _ _ bs cs ) _ fs=undefined

addInternals :: ArchElem a -> ([Wire a],[ArchElem a]) -> ArchElem a
addInternals (Function q w e r _ a) (ws,as)=Function q w e r (as,ws) a
addInternals _ _=error "can not add internals to an architecture element that is not a Function"

parseSignalDecsOf :: BlockDecItem -> EnvSession (Maybe (String,Port))
parseSignalDecsOf (BDISPB s)=do return Nothing --wordt later ergens anders geparsed
parseSignalDecsOf (BDISD s) =do
  parsedSigs <- parseSigDec s
  return (Just parsedSigs)

parseSigDec :: SigDec -> EnvSession (String,Port)
parseSigDec (SigDec id t Nothing) = do
  types <- MonadState.get types
  let found = lookup t types
      portId=parseId id
      Just getPort = found
      result
        | isNothing found = (portId,SinglePort (portId ++ " Could not find type:" ++ show t)) 
          --dit is niet helemaal correct, er treed hier soms een fout op dat boolean (en dus ook mogelijke andere dingen) niet gevonden kan worden
        | otherwise = (portId, getPort portId)
  return result
parseSigDec (x@(SigDec id t (Just expr)))=do return ("het volgende kan nog niet geparsed worden: " ++ (show x),SinglePort "error")

----------------------------------------------------------------------------------------------------

parseConcSm :: ConcSm -> [(String,Port)] -> EnvSession (ArchElem (), Backtrack)
parseConcSm (CSBSm x) portTable = parseBlockSm x portTable
parseConcSm (CSSASm (s :<==: x)) portTable = do 
  result <- parseConWforms (parseVHDLName s) portTable x
  return (PortReference $ SinglePort (parseVHDLName s), result) 

parseConcSm (CSISm (CompInsSm _ insUnit (PMapAspect pMapAspect))) portTable = do
  function <- parseInsUnit insUnit
  let filteredAssocElems = filter (\((Just id) :=>: _) -> fromVHDLId id `notElem` ["clock","resetn"]) pMapAspect
      wires = parsePMapAspect filteredAssocElems
      result = (PortReference $ SinglePort (getHighest $ outportOf function) , (Backtrack function wires []))
  return result

parseConcSm (CSPSm x) portTable=undefined
parseConcSm (CSGSm x) portTable=undefined

-- Describes an Element
parseConWforms :: String -> [(String,Port)] -> ConWforms -> EnvSession Backtrack
parseConWforms s portTable (ConWforms [] (Wform f) Nothing) = parseWformElems s portTable f
-- Describes a Mux
{-}
parseConWforms s portTable (ConWforms x f Nothing)
  |length selects /= 0  = result
  |otherwise           = error  "no result from the parsed whenElses statements, please fix parseConWforms."
    do
      n1 <- getNewId
      n2 <- getNewId
      n3 <- getNewId
      n4 <- getNewId
      --a Mux without its in- and outgoing wires: 
      let typePort= sureLookup s portTable
          outPort= portLike (newPortId n1) typePort
          inportNames= [portLike (newPortId (number+n2)) typePort|number<- [1..totalIns]]  --old:[newPortId (number+n) |number<- [1..totalIns]]
          totalIns=(length x+1)
          totalSelects=length x
          selectNames= [portLike (newPortId (number+n3)) typePort |number<- [1..totalSelects]]    --old:[newPortId (number+secondN) |number<- [1..totalSelects]]

       -- Subcompoenents need to be parsed:
           parsedWhenElses= parseWhenElses s portTable x
           otherwiseUitgang=parseWform s portTable f 
           (ins,selects)=unzip (snd parsedWhenElses) 
       
       --some wires between the subcomponent and the new mux need to be made:
           tempResult=connect ((concat ins) ++ otherwiseUitgang) currMux "a mux input wire" --select entrances still need to be linked here -- is last hier zo goed?
           currMux=Mux (operatorId n4) inportNames outPort selectNames ()

           trueResult=connectSelects selects tempResult "a select mux wire"
           result = [trueResult]

parseWhenElses:: String -> [(String,Port)] -> [WhenElse] -> EnvSession [([Backtrack],Backtrack)]
parseWhenElses s portTable xs = mapAccumL (parseWhenElse s portTable) xs

parseWhenElse:: String -> [(String,Port)] -> WhenElse -> EnvSession ([Backtrack],Backtrack)
parseWhenElse s portTable (WhenElse wform expr) = do
  resultWform <- parseWform s portTable wform
  resultGaurd <- parseExpr s portTable expr
  --volgens mij is resultWform altijd maar 1 element
  return (resultWform,resultGaurd)


-}
--verbind de meegegeven geparse delen aan het meegegeven architectuurElement.
connect:: [Backtrack] -> ArchElem () -> String -> Backtrack
connect xs m name = (Backtrack m allWires allElems)
  where
    newWires=map makeNewWire (zip fromOuts toIns)
    fromOuts=map (getHighest.outportOf.archElem) xs
    toIns=map getHighest (inportsOf m)
    allElems=(map archElem xs) ++ concat (map prevArchElems xs)
    allWires=newWires ++ concat (map wires xs)
    makeNewWire (x,i)=Wire (Just name) x i ()

--lijkst heel sterk op connect, misschien 1 algemenere functie maken die beide afhandeld?
-- nog niet monadisch herschreven --------
{-}
connectSelects xs ((mux@(Mux _ _ _ selectNames _)),wires,elems,n,m) name
     =(mux,allWires,allElems,finalN,finalM)
       where
        newWires=map makeNewWire (zip (map (getHighest.outportOf.get1out5) xs) (map getHighest selectNames))
        allElems=(map get1out5 xs) ++ concat (map get3out5 xs) ++ elems
        allWires=newWires ++ concat (map get2out5 xs) ++ wires
        finalN= get4out5 (last xs)
        finalM= get5out5 (last xs)
        makeNewWire (x,i)=Wire (Just name) x i ()
-}
------------------------ END MUXES ------------------------------------


-- We expect only one WFormElem
parseWformElems :: String -> [(String,Port)] -> [WformElem] -> EnvSession Backtrack
parseWformElems s portTable [] = undefined
parseWformElems s portTable ((WformElem f Nothing):_) = parseExpr s portTable f

parseVHDLName (NSimple s)     =parseSimpleName s
parseVHDLName (NSelected s)   =parseSelectedName s
parseVHDLName (NIndexed s)    =parseIndexedName s
parseVHDLName (NSlice s)      =parseSliceName s
parseVHDLName (NAttribute s)  =parseAttibName s

parseSimpleName ::SimpleName-> Id
parseSimpleName  s=parseId s

parseSelectedName::SelectedName-> Id
parseSelectedName (x :.: y)=(parsePrefix x) ++ "." ++ (parseSuffix y)

parsePrefix x=parseVHDLName x

parseSuffix:: Suffix -> Id
parseSuffix (SSimple s)=parseSimpleName s
parseSuffix (All)=""

parseIndexedName (IndexedName x es)=undefined
parseSliceName s=undefined
parseAttibName s=undefined

--- EXPRESSIONS ------------------

parseExpr :: String -> [(String,Port)] -> Expr -> EnvSession Backtrack
parseExpr s portTable (PrimFCall x) = parseFCall s portTable x

parseExpr s portTable (PrimLit c)= do
  n1 <- getNewId
  n2 <- getNewId
  return (Backtrack (Literal ("lit" ++ operatorId n1) c (portLike (newPortId n2) (sureLookup s portTable)) ()) [] [])

--verwijst naar de meegegeven VHDL naam. Kan in een latere iteratie worden weggehaald
--mogelijk dient ook hier PortLike gebruikt worden..
parseExpr s portTable (PrimName x)  = do 
  return (Backtrack (PortReference (SinglePort (parseFName x))) [] [])

--we have not come across this type in one of our examples yet. We will implement it when we encounter it.   
parseExpr s portTable (Aggregate eas)=undefined 

parseExpr s portTable (And x y)   =parseBinExpr s portTable "and" x y
parseExpr s portTable (Or x y)    =parseBinExpr s portTable "or" x y
parseExpr s portTable (Xor x y)   =parseBinExpr s portTable "xor" x y
parseExpr s portTable (Nand x y)  =parseBinExpr s portTable "nand" x y
parseExpr s portTable (Nor x y)   =parseBinExpr s portTable "nor" x y
parseExpr s portTable (Xnor x y)  =parseBinExpr s portTable "xnor" x y
parseExpr s portTable (x :=: y)   =parseBinExpr s portTable "=" x y
parseExpr s portTable (x :/=: y)  =parseBinExpr s portTable "/=" x y
parseExpr s portTable (x :<: y)   =parseBinExpr s portTable "<" x y
parseExpr s portTable (x :<=: y)  =parseBinExpr s portTable "<=" x y
parseExpr s portTable (x :>: y)   =parseBinExpr s portTable ">" x y
parseExpr s portTable (x :>=: y)  =parseBinExpr s portTable ">=" x y
parseExpr s portTable (x :+: y)   =parseBinExpr s portTable "+" x y

{- UnairyExpr moet nog monadisch worden herschreven ----
parseExpr s portTable (Neg x) n m     =parseUnairyExpr s portTable "neg" x n m
parseExpr s portTable (Pos x) n m     =parseUnairyExpr s portTable "pos" x n m
-}
--andere expressies gaan soortgelijk..
--Dus soortgelijk kan gedaan worden voor Adding Operators,Multiplying Operators en Shift Operators en Miscellaneous Operators

parseBinExpr:: String -> [(String,Port)] -> String -> Expr -> Expr -> EnvSession Backtrack
parseBinExpr s portTable name x y = do
  n1 <- getNewId
  n2 <- getNewId
  n3 <- getNewId
  n4 <- getNewId
  subOpX <- parseExpr s portTable x 
  subOpY <- parseExpr s portTable y
  let currOperator=Operator (operatorId n1) name ins out ()
      in1=portLike (newPortId n2) portType
      in2=portLike (newPortId n3) portType
      ins=[in1,in2]
      out=portLike (newPortId n4) portType
      parsedsubOps=[subOpX,subOpY]
      portType=sureLookup s portTable 
      --although this is a valid Port, it may not be unique. therefoe we use a portLike instead of this excact port for the outport.
      result=connect parsedsubOps currOperator "an expression wire" 
      --Ik gebruik nu ook hier connect i.p.v. de dingen expliciet uit te schrijven omdat dit algemener is en meer compositioneel is, wat in het onderhoudt mogelijk handig is.
  return result

----- nog niet monadisch herschreven ---------
{-}
parseUnairyExpr :: String -> [(String,Port)] -> String -> Expr -> Int -> Int -> (ArchElem (),[Wire ()],[ArchElem ()],Int,Int)
parseUnairyExpr s portTable name x n m=result 
  where
    currOperator=Operator (operatorId m) name ins out ()
    ins=[portLike (newPortId n) portType]
    out=portLike (newPortId (n+1)) portType

    parsedsubOp=[parseExpr s portTable x (n+2) (m+1)]

    portType=sureLookup s portTable --although this is a valid Port, it may not be unique. therefoe we use a portLike instead of this excact port for the outport.
    result=connect parsedsubOp currOperator "an expression wire" --Ik gebruik nu ook hier connect i.p.v. de dingen expliciet uit te schrijven omdat dit algemener is en meer compositioneel is, wat in het onderhoudt mogelijk handig is.

-}
-- ENORME HACK - Dit is alleen voor een Literal. Er wordt niets met de tweede Primlit gedaan.
parseFCall s portTable (FCall (NSimple (Basic "to_signed"))
  [Nothing :=>: ADExpr (PrimLit x)
  ,Nothing :=>: ADExpr (PrimLit y)])
  = parseExpr s portTable (PrimLit x)
  --dit laat de to_signed functie en diens tweede argument weg, is in princiepe niet nodig aangezien dit ook later bij de GUI gedaan kan worden (nu gaat data van het type verloren), maar geeft een netter uitzient resultaat.

--dit deel werkt niet. Dit dient nog aangepast te worden:
{-}
parseFCall s portTable (FCall functionName assocElems) n m =
      (Function newId (Just fName) (map SinglePort inports) outport ([],[]) () 
      --aan de hand van de naam van de functie: fName, kunnen de inwendige componenten worden opgezocht wanneer nodig
      ,[Wire Nothing (associatedOutports!!i) (inports!!i) ()| i<- [0..(inputLength-1)] ] ++ concat (map get2out5 subParse)
      ,concat(map get3out5 subParse)
      ,n+inputLength
      ,m+1
      )
    where 
      fName= parseFName functionName
      newId= fName ++ operatorId m
      subParse=[] 
      --TODO mappen met doorgeven n en m : map (parseAssocElem n+inputLength m+1) s portTable assocElems
      inputLength= length subParse 
      --Als in subParse ook een uitvoer, wordt dit 1 minder en worden andere dingen ook wat ingewikkelder, ik neem hier aan dat dit niet het geval is, omdat ik het niet weet en ik niet graag onnodig werk doe.
      inports=[newPortId (i+n)|i<- [0..(inputLength-1)]]
      outport=SinglePort $ newPortId (n) 
      associatedOutports=map outOf subParse
      --mogelijk afhankelijk van subParse (als daar een uitvoernaam bijzit)? Zo ja, later aanpassen

parseAssocElem :: String -> [(String,Port)] -> AssocElem -> Int -> Int -> (ArchElem (),[Wire ()],[ArchElem ()],Int,Int)
parseAssocElem s portTable (Nothing :=>: (Open)) n m=  (PortReference (SinglePort ("Nothing :=>: (Open) kan nog niet geparsd worden")),[],[],n,m)
parseAssocElem s portTable (Nothing :=>: (ADExpr e)) n m= parseExpr s portTable e n m
parseAssocElem s portTable (Nothing :=>: (ADName x)) n m= parseExpr s portTable (PrimName x) n m
--Hiervoor moet x waarschijnlijk opgezocht kunnen worden en moet dus mogelijk meer informatie aan parseExpr worden meegegeven:
parseAssocElem s portTable (Just x :=>: y) n m = (PortReference (SinglePort ("Just _ :=>: _ kan nog niet geparsd worden")),[],[],n,m)
-}
--ik heb deze methode zonder het in te zien 2 keer geschreven.. zie parseVHDLName in ParseVHDLvoor z´n duplicaat...:
parseFName :: VHDLName -> String
parseFName (NSimple x)                 = parseId x
parseFName (NSelected (vhdlName :.: (SSimple id))) = (parseFName vhdlName) ++ "." ++ parseId id
parseFName (NIndexed x)                = "kan nog niet geparsd worden" ++ show (x)
parseFName (NSlice x)                  = "kan nog niet geparsd worden" ++ show (x)
parseFName (NAttribute x)              = "kan nog niet geparsd worden" ++ show (x)

--deze methode is oud en zal waarschijnlijk niet meer nodig zijn..:
outOf::ArchElem a-> PortId
outOf (Operator q w es r t)=extract (r)
  where 
    extract (SinglePort x)=x

outOf (Literal q v o a)=extract (o)
  where 
    extract (SinglePort x)=x
    
outOf (PortReference p)=extract (p)
  where 
    extract (SinglePort x)=x
    extract (MultiPort x [y])= extract y  
    --dit werkt voor de huidige manier van selected names parsen

parseId::  VHDLId-> Id
parseId s=fromVHDLId s

--ja, de volgende functies zijn triviaal en oorspronkelijk bedoeld als placeholders. Echter is er wijnig reden ze aan te passen op het huidige moment van schrijven:
operatorId::Int -> String
operatorId m= "operatorId" ++ show m

newPortId m= "newPortId" ++ show m




------------------------ Functions in Functions ---------------

parseInsUnit :: InsUnit -> EnvSession (ArchElem ())
parseInsUnit (IUEntity name) = do 
  let filename = parseVHDLName name
  vhdl <- searchVHDLsById filename
  parseDesignFile vhdl

parsePMapAspect :: [AssocElem] -> [Wire ()]
parsePMapAspect [] = []
parsePMapAspect (a@((Just start) :=>: ADExpr (PrimName (NSimple destination))):ass) = result
  where
    result = (Wire Nothing (parseSimpleName start) (parseSimpleName destination) ()) : parsePMapAspect ass

-- Searches the list of rendered VHDL files by clash for the 
-- designfile that has the given id
searchVHDLsById :: String -> EnvSession DesignFile
searchVHDLsById s = do
  vhdls <- MonadState.get vhdlFiles
  let vhdlTable = map (\(id, df) -> (fromVHDLId id, df)) vhdls
      entry = lookup s vhdlTable
      result 
        |isNothing entry = error $ "Designfile the following Designfile is missing: " ++ s
        |otherwise = (\(Just df) -> df) entry
  return result


---------------------------- Portreferences & associations ---------


removeReferences :: ([Wire ()],[ArchElem ()]) -> [(ArchElem (),Backtrack)] -> [String] -> ([Wire ()],[ArchElem ()])
removeReferences (ws,(a@(PortReference (SinglePort x)):as)) table ins
  | x `elem` ins = (fst niksVeranderd, snd niksVeranderd) --HACKED??
  | otherwise = (fst r , snd r)
    where
      r=  removeReferences ((ws  \\ [w] )  ++ (fst newReferences),(snd newReferences) ++ as ) table ins --TODO: dit geeft een oneindige lus als een element dat al bestond weer wordt toegevoegd, check of union i.p.v. concatteneren hier correct resultaat opleverd..
      newReferences=resolveAssociationNamed table ins i x --mogelijk moeten alle associaties eerder worden verholpen om i te kunnen vinden, dan krijgen we de error in findInof..
      (i,w)= findInof x ws  --w dient nu verwijderd te worden (het nesten van signalen wordt nl niet toegestaan)
      niksVeranderd=  removeReferences (ws,as) table ins
      --Just res = lookup a table
      --i = getHighest $ outportOf$ fst5 res
removeReferences (ws,(PortReference (MultiPort _ _)):as) table ins = undefined --zal dit ooit voorkomen?
  where  
    r= removeReferences (ws,as)
removeReferences (ws,(a:as)) table ins = (fst r, a: snd r)
  where  
    r= removeReferences (ws,as) table ins
removeReferences (ws,[]) table ins = (ws,[])

findInof :: PortId -> [Wire a] -> (PortId,Wire a)
findInof p []=error $ "kan " ++ show p ++ " niet vinden, blijkbaar moeten alle wires eerst worden gevonden aangezien er nu 1 mist.."
findInof p ((w@(Wire name x y _)):ws)
  |name /= Nothing && n == x = (y,w)
  |x==p       = (y,w)
  |otherwise = findInof p ws
  where
    Just n = name
    
myGeneralizedLookup :: ArchElem () -> [(ArchElem (),Backtrack)] -> [(PortId,Backtrack)]
myGeneralizedLookup (PortReference (SinglePort x)) []=[]
myGeneralizedLookup (r@(PortReference (SinglePort x))) (((PortReference (SinglePort y)),result):ps)
   |(untillDot x) == (untillDot y) = (y,result) : (myGeneralizedLookup r ps)
   |otherwise                     = (myGeneralizedLookup r ps)

resolveassociation ::  [(ArchElem (),Backtrack)] -> [String] -> String -> ([Wire ()],[ArchElem ()])
resolveassociation table ins i =resolveAssociationNamed table ins i i

resolveAssociationNamed :: [(ArchElem (), Backtrack)] -> [String] -> String -> String -> ([Wire ()],[ArchElem ()])
resolveAssociationNamed table ins outName x --x is a signaalname that can be found in a PortReference
-- HACKED, Door de error weg te halen en de informatie uit het Backtrack element terug te geven, werkt het Function in function voorbeeld
-- |(allRelated == []) && (not (isIn (untillDot x))) = error $ "We kunnen " ++ x ++ " niet vinden: "++ show tableOut++"\n  ins are:" ++ (show ins)
  |(allRelated == []) && (not (isIn (untillDot x))) = (wires (snd(head table)),[archElem (snd(head table))])
  |otherwise =result
   where
     toBeResolvedReference=(PortReference $ SinglePort x)
     allRelated=myGeneralizedLookup toBeResolvedReference table -- TODO: x kan iets zijn als naam.A , als dat het geval is moet op naam worden gezocht. Ook kan het zijn dat als x naam is er in de tabel bijvoorbeeld een naam.A en een naam.B staat.
     exactFound=lookup x allRelated
     Just exact=exactFound
     currRess | exactFound == Nothing = (map snd allRelated)  --neemt gewoon alles
              | otherwise =  [exact]
             --Als we naar iets zoeken wat we ergens al excact geparsed hebben, gaan we geen overbodige extra componenten opleveren.
             --Dus als we hebben ¨x <- naam.B, naam.B <- y¨ gaan we meteen naar het resultaat y en kijken we niet naar resultaten van bv naam.A parsen.
     result |isIn (untillDot x)            =([inWire],[])
            |otherwise                     =concatted
     isIn y= elem y ins
     inWire=Wire (Just x) x outName ()
     concatted = (concat $ fst unzipped, concat $ snd unzipped)
     unzipped = unzip checkAll
     checkAll = map (resolveFoundAssociation table ins outName x) currRess
     -- Hier gebleven met herschrijven


resolveFoundAssociation table ins outName x currRes
   = result
    where
     result |not (checkIsReference firstElem)=(newWire: (wires currRes),(archElem currRes : prevArchElems currRes))
            |otherwise                     =solveRecursivly
     firstElem=archElem currRes
     newWire = Wire (Just x) wireStartId outName ()
     wireStartId= (untillDot (getHighest(outportOf firstElem))) ++ (fromdot x)
     newX=getHighest(outportOf firstElem)
     solveRecursivly = ((fst recursivlyResolved) ++ (wires currRes) , (snd recursivlyResolved) ++ (prevArchElems currRes)) --signalen mogen niet rechtstreeks recursief zijn opgescheven, omdat anders hier een oneindige loop ontstaat. Dus geen rechtstreekse a<- b, b<- a of varianten hierop. recursie van signalen binnen elementen zoals registers zal hier geen probleem geven.
     recursivlyResolved=resolveAssociationNamed table ins outName newX

isInSignal (PortReference (SinglePort x)) ins  = elem x ins
isInSignal (PortReference (MultiPort _ _)) ins = undefined --kan nu nog niet voorkomen in prototype..

checkIsReference:: ArchElem a -> Bool
checkIsReference (PortReference z)=True
checkIsReference _=False

inSignalsOf:: ArchElem a -> [String]
inSignalsOf (Function _ _ ins _ _ _)=parseToSingles ins
outSignalsOf:: ArchElem a -> [String]
outSignalsOf (Function _ _ _ out _ _)=parseToSingles [out]

parseToSingles::[Port]-> [String]
parseToSingles []=[]
parseToSingles ((SinglePort x):xs) =x:parseToSingles xs
parseToSingles ((MultiPort y ys):xs)= [y] ++ (parseToSingles ys) ++ (parseToSingles xs)

searchFunction:: String -> [ArchElem a] -> ArchElem a
searchFunction s (f@(Function x _ _ _ _ _):fs)
  |x==s        =f
  |otherwise   = searchFunction s fs
searchFunction s []=error $ "a functie with the name" ++ s ++ "was not found by the parser. We might have messed up, sorry for the inconvienience.."
searchFunction s _=error  "the impossible happened" --De architectuurelementen in de invoer horen nl allemaal Funtions te zijn...

-------- HELPER -------

-- Get the outport of an Architecture element
outportOf :: ArchElem a -> Port
outportOf (Function _ _ _ p _ _) = p
outportOf (Operator _ _ _ p _) = p
outportOf (Literal _ _ p _) = p
outportOf (Mux _ _ p _ _) = p
outportOf (Register _ _ p _) = p
outportOf (PortReference p) = p

inportsOf :: ArchElem a -> [Port]
inportsOf (Mux _ inportNames _ _ _)     = inportNames
inportsOf (Operator _ _ inportNames _ _) = inportNames

getHighest::Port -> PortId
getHighest (SinglePort x)=x
getHighest (MultiPort x _)=x

--helpfunctie, ik vindt het overzichtelijker de error hier op te vangen:
sureLookup t table
  | isNothing found = error $ "Could not find this:" ++ show t
  | otherwise = x
    where
    found = lookup t table
    Just x = found

--helpfuntie, deze maakt een poort die qua structuur lijkt op de eerste, maar dan met de meegegeven naam
portLike :: String -> Port -> Port
portLike name (SinglePort id)   =SinglePort (name ++ (fromdot id))
portLike name (MultiPort id ps) =MultiPort (name ++ (fromdot id)) (map (portLike name) ps)

fromdot :: String -> String
--geeft alles vanaf de eerste punt van de string op; zoals .A.B vanuit naam.A.B
fromdot []=[]
fromdot (s@(('.'):sx))=s
fromdot (x:xs)=fromdot xs

untillDot :: String -> String
--geeft alles tot de eerste punt van de string op; zoals naam vanuit naam.A.B
untillDot []        =[]
untillDot (('.'):sx)=[]
untillDot (x:xs)    =x: (untillDot xs)

{-
  A state (register)
-}
parseBlockSm :: BlockSm -> [(String,Port)] -> EnvSession (ArchElem (), Backtrack)
parseBlockSm b@(BlockSm l _ _ _ _) portTable
  | label == "state" = parseState b portTable
  | otherwise = error $ "can't yet parse BlockSm with label " ++ label
  where label = fromVHDLId l
