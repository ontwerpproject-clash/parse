--------------------------------------------------------
-- Parser VHDL to Datastruct
--------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module ParseVHDL where

-- Standard imports
import Data.Maybe (isNothing,catMaybes)
import Data.List
import qualified Control.Monad.Trans.State as State
import qualified Data.Accessor.Monad.Trans.State as MonadState
import qualified Data.Accessor.Template
import qualified Data.Set as Set

-- VHDL Imports
import Language.VHDL.AST hiding (Function)

-- Local Imports
import Helper
import Datastruct
import ParseTypes
import {-# SOURCE #-} ParseState

--------------------------------------------------------
-- Parse Designfile & Entity
--------------------------------------------------------

-- | ParseVhdlAsts is the main function of the parser.
-- | The functions starts a session and initialises the Environment State
-- | in which all the global variables are stored. The runSession returns
-- | a tuple of buildup ArchElem and the end State.
-- | 
-- | input: Output of clash; a list of Names and Designfiles
-- | output: An ArchElem based on the Designfiles from the input
parseVhdlAsts :: [(VHDLId, DesignFile)] -> ArchElem ()
parseVhdlAsts vhdls
  = fst (runSession (initEnvironment vhdls types) initState)
  where
    typesAst = head vhdls
    -- TypesTable is buildup for future referencing
    types = parseTypes typesAst

-- | The Environment State is initialized with the global values
-- | of the vhdl files and the types table. The Counter is set to 0.
-- | This is the last function in the chain of monadic function, as it returns
-- | the final Environment Session variable.
initEnvironment :: [(VHDLId, DesignFile)] -> Types -> EnvSession (ArchElem ())
initEnvironment vhdls settypes = do
    MonadState.set counter 0
    MonadState.set vhdlFiles vhdls
    MonadState.set types settypes
    let topentity = vhdls !! 1
    parseTopEntity topentity

-- Takes the DesignFile from the VHDL table
parseTopEntity :: (VHDLId, DesignFile) -> EnvSession (ArchElem ())
parseTopEntity (id,df) = parseDesignFile(df)

-- Ignores the ContextItems and passes through the Entity
parseDesignFile :: DesignFile -> EnvSession (ArchElem ())
parseDesignFile (DesignFile contextItems ls) = parseEntity ls

parseEntity :: [LibraryUnit] -> EnvSession (ArchElem ())
parseEntity ((LUEntity e):ls) = parseEntityDec e ls 

-- | The EntityDec is the first LibraryUnit that is processes. EntityDec holds
-- | the in- and outports of the main function; the hardware. The parsed EntityDec
-- | combined with an empty Function element is used to parse the next LibraryUnit
-- | the LUArch, which holds the Architecture body. 
-- | Instead of returning just the EntityDec it, passes it on to parse more
-- | LibraryUnits. 
-- | Current function only parses EntityDec and LUArch, it doesn't process 
-- | LUPackageDec and LUPackageBody. This could be implemented in the future,
-- | uptill now there are no examples using these LibraryUnits.
parseEntityDec :: EntityDec -> [LibraryUnit] -> EnvSession (ArchElem ())
parseEntityDec (EntityDec id sigs) ls = do
    let isLUArch (LUArch _) = True
        isLuArch _          = False
        (LUArch archbody):_ = filter isLUArch ls
        -- The clock and resetn signal are ignored in the visualisation tool. Therefore they're removed.
        -- Get the port id's on the Top entity, no difference between in- and outports.
        parsedSigs = filter (\(IfaceSigDec id _ _) -> fromVHDLId id `notElem` ["clock","resetn"]) sigs
    -- returns a list of Ports and a boolean is set true for the inports.
    ports <- mapM (parseIfaceSigDec) parsedSigs
    let ins = map fst $ filter snd ports
    let out = head $ map fst $ filter (not . snd) ports
    -- List of Port id's and a boolean whether it is an inport.
    portTable <- mapM (buildPortTableEntry) parsedSigs
    -- Empty function that is the base of the Architecture that is buildup during the parsing process.
    let emptyfunction = Function (parseId id) Nothing ins out ([],[]) ()
    result <- parseArchBody archbody portTable [emptyfunction]
    return result

-- | Parses an IfaceSigdec, interface signal declaration. It checks whether the
-- | described Port is an Inport, and is returned in the tuple.
-- | output:  Tuple of described Port element and Inport boolean
-- |            True -> Inport, False -> Outport
-- | error:   On unidentified type of the signal declaration
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

-- | Given a signal declaration, it returns a tuple of the
-- | id of the string and the corresponding Port.
-- | output:  Tuple of port id as a String, and Port element
-- | error:   On unidentified type of the signal declaration
buildPortTableEntry :: IfaceSigDec -> EnvSession (String, Port)   
buildPortTableEntry (IfaceSigDec id _ t) = do
    typeTable <- MonadState.get types
    let found = lookup t typeTable
        Just getPort = found
        result
          | isNothing found = error $ "Could not find type:" ++ show t
          | otherwise = ((parseId id), getPort (parseId id))
    return result
     
--------------------------------------------------------
-- Parsing Architecture body
--------------------------------------------------------

-- | The ArchBody holds all the architecture of the Entity. It consists of
-- | a name, a list of Block declarative items and a list of concurrent
-- | statements: ArchBody VHDLId VHDLName [BlockDecItem] [ConcSm].
-- | The block outports from the BlockDecItems are added to the portTable, and
-- | Conc statements are parsed and return Backtrack elements.
-- | These Backtrack elements are resolve, complete the PortReferences, and
-- | are combined to one Architecture element.
parseArchBody :: ArchBody -> [(String,Port)] -> [ArchElem ()] -> EnvSession (ArchElem ())
parseArchBody (ArchBody (Basic "structural") (NSimple x) bs cs ) portTable fs = do
  types <- MonadState.get types
  -- Outports of architecture blocks, these are added to the Port table
  signalDecs <- mapM (parseSignalDecsOf) bs
  let pTable=portTable ++ catMaybes signalDecs 
      -- For better reading, myParseConcSm is writen, with type declaration
      myParseConcSm :: ConcSm -> EnvSession (ArchElem (), Backtrack )
      myParseConcSm c = parseConcSm c pTable
  parsedCs <- mapM (myParseConcSm) cs
  let -- In order to detect loops a boolean is added to the Backtrack element
      parsedCsTable :: LookupTable2
      parsedCsTable = map (\(a,b) -> (a,Backtrack2{bt=b,seen=False})) parsedCs
      -- To resolve the references we start at the OutPort of the top level Function
      currentArchElem=searchFunction (parseId x) fs
      myResolveassociation :: LookupTable2 -> String -> (LookupTable2,([Wire ()],[ArchElem ()]))
      myResolveassociation myTable = resolveassociation myTable (inSignalsOf currentArchElem)
      outsResolved  :: [([Wire ()],[ArchElem ()])]
      (parsedCsTableNew, outsResolved)=mapAccumL myResolveassociation parsedCsTable (outSignalsOf currentArchElem)
      -- All Wires are constructed by resolving all the PortReference associations
      unzipped = unzip outsResolved
      concatted :: ([Wire ()],[ArchElem ()])
      concatted= (concat $ fst unzipped, concat $ snd unzipped)
      -- Removing PortReferences, should not be in final ArchElem
      internals=removeReferences concatted parsedCsTableNew (inSignalsOf currentArchElem)
      -- Final step in a DesignFile. Final lists of ArchElems and Wires are added.
      newElem= addInternals currentArchElem internals
  return newElem
parseArchBody (ArchBody _ (NSimple x) bs cs ) _ fs=undefined
parseArchBody (ArchBody (Basic "structural") _ bs cs ) _ fs=undefined
parseArchBody (ArchBody _ _ bs cs ) _ fs=undefined

-- | Insert all the ArchElems and Wires into the Function.
addInternals :: ArchElem a -> ([Wire a],[ArchElem a]) -> ArchElem a
addInternals (Function q w e r _ a) (ws,as)=Function q w e r (as,ws) a
addInternals _ _=error "can not add internals to an architecture element that is not a Function"

-- | Only parses Signal declarations.
parseSignalDecsOf :: BlockDecItem -> EnvSession (Maybe (String,Port))
parseSignalDecsOf (BDISPB s)=do return Nothing
parseSignalDecsOf (BDISD s) =do
  parsedSigs <- parseSigDec s
  return (Just parsedSigs)

-- | First is checked if the Signal declaration is in the Types Table. The
-- | table returns the proper Port element.
-- | Output: 2 Tuple of the port id of the signal, and the Port element
parseSigDec :: SigDec -> EnvSession (String,Port)
parseSigDec (SigDec id t Nothing) = do
  -- Get types Table from the Environment
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

-- | The concurrent statements describe the actual Architecture elements
-- | in a Function. There are these types of statements:
-- |    CSBSm BlockSm           Describes a State
-- |    CSSASm ConSigAssignSm   Describes a Signal assignment, usual Operaters, Muxes and Literals
-- |    CSISm CompInsSm         Describes a lowerlevel architectures, Function in a Function
-- |    CSPSm ProcSm            Is not used at the moment
-- |    CSGSm GenerateSm        Is not used at the moment
-- |
-- | After parsing the statement it return a PortReference which is needed
-- | to link the ArchElem to the Function. The Backtrack record holds all elements
-- | that need to be added to the Function.
-- | Output: 2 Tuple of a PortReference and a Backtrack record
parseConcSm :: ConcSm -> [(String,Port)] -> EnvSession (ArchElem (), Backtrack)
parseConcSm (CSBSm x) portTable = parseBlockSm x portTable
parseConcSm (CSSASm (s :<==: x)) portTable = do 
  result <- parseConWforms (parseVHDLName s) portTable x
  return (PortReference $ SinglePort (parseVHDLName s), result) 

parseConcSm (CSISm (CompInsSm _ insUnit (PMapAspect pMapAspect))) portTable = do
  function <- parseInsUnit insUnit
  let toResolve = (getHighest $ outportOf function)
      filteredAssocElems = filter (\((Just id) :=>: _) -> fromVHDLId id `notElem` ["clock","resetn","toResolve"]) pMapAspect
      (resolve,wires,refs) = parsePMapAspect filteredAssocElems
      result = (resolve, (Backtrack function wires refs))
  return result

parseConcSm (CSPSm x) portTable=undefined
parseConcSm (CSGSm x) portTable=undefined

--------------------------------------------------------
-- Parsing Mux element
--------------------------------------------------------

parseConWforms :: String -> [(String,Port)] -> ConWforms -> EnvSession Backtrack
parseConWforms s portTable (ConWforms [] (Wform f) Nothing) = parseWformElems s portTable f
-- Describes a Mux

parseConWforms s portTable (ConWforms x f Nothing) = do
  n1 <- getNewId
  n2 <- getNewId
  n3 <- getNewId
  n4 <- getNewId
  --a Mux without its in- and outgoing wires: 
  let typePort= sureLookup s portTable
      outPort= portLike ("newPortId" ++ n1) typePort
      totalIns=(length x+1)
      totalSelects=length x
  inIds <- mapM (\_ -> getNewId) [1..totalIns]
  selectIds <- mapM (\_ -> getNewId) [1..totalSelects]
  let inportNames= [portLike ("newPortId" ++ n) typePort|n <- inIds]
      selectNames= [portLike ("newPortId" ++ n) typePort |n <- selectIds]

   -- Subcompoenents need to be parsed:
  parsedWhenElses <- parseWhenElses s portTable x
  otherwiseExit <- parseWform s portTable f 
  let (ins,selects) = unzip parsedWhenElses 
      --some wires between the subcomponent and the new mux need to be made:
      tempResult=connect (ins ++ [otherwiseExit]) currMux "a mux input wire" --select entrances still need to be linked here -- is last hier zo goed?
      currMux=Mux ("operatorId" ++ n4) inportNames outPort selectNames ()

      trueResult=connectSelects selects tempResult "a select mux wire"
      result
        |length selects /= 0  = trueResult
        |otherwise            = error  "no result from the parsed whenElses statements, please fix parseConWforms."
  return result
        
parseWhenElses:: String -> [(String,Port)] -> [WhenElse] -> EnvSession [(Backtrack,Backtrack)]
parseWhenElses s portTable xs = mapM (parseWhenElse s portTable) xs

parseWhenElse:: String -> [(String,Port)] -> WhenElse -> EnvSession (Backtrack,Backtrack)
parseWhenElse s portTable (WhenElse wform expr) = do
  resultWform <- parseWform s portTable wform
  resultGaurd <- parseExpr s portTable expr
  --volgens mij is resultWform altijd maar 1 element
  return (resultWform,resultGaurd)

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
connectSelects :: [Backtrack] -> Backtrack -> String -> Backtrack
connectSelects xs c name = Backtrack mux allWires allElems
  where
    (mux@(Mux _ _ _ selectNames _)) = archElem c
    newWires=map makeNewWire (zip (map (getHighest.outportOf.archElem) xs) (map getHighest selectNames))
    allElems=(map archElem xs) ++ concat (map prevArchElems xs) ++ prevArchElems c
    allWires=newWires ++ concat (map wires xs) ++ wires c
    makeNewWire (x,i)=Wire (Just name) x i ()

------------------------ END MUXES ------------------------------------

parseWform :: String -> [(String, Port)] -> Wform -> EnvSession Backtrack
parseWform s portTable (Wform f) = parseWformElems s portTable f 

-- We expect only one WFormElem
parseWformElems :: String -> [(String,Port)] -> [WformElem] -> EnvSession Backtrack
parseWformElems s portTable [] = undefined
parseWformElems s portTable ((WformElem f Nothing):_) = parseExpr s portTable f

parseVHDLName :: VHDLName -> Id
parseVHDLName (NSimple s)     =parseSimpleName s
parseVHDLName (NSelected s)   =parseSelectedName s
parseVHDLName (NIndexed s)    =parseIndexedName s
parseVHDLName (NSlice s)      =parseSliceName s
parseVHDLName (NAttribute s)  =parseAttibName s

parseSimpleName ::SimpleName-> Id
parseSimpleName  s=parseId s

parseSelectedName::SelectedName-> Id
parseSelectedName (x :.: y)=(parsePrefix x) ++ "." ++ (parseSuffix y)

parseIndexedName (IndexedName x es)=undefined
parseSliceName s=undefined
parseAttibName s=undefined

parsePrefix :: Prefix -> Id
parsePrefix x=parseVHDLName x

parseSuffix:: Suffix -> Id
parseSuffix (SSimple s)=parseSimpleName s
parseSuffix (All)=""

--------------------------------------------------------
-- Parsing Expressions
--------------------------------------------------------

parseExpr :: String -> [(String,Port)] -> Expr -> EnvSession Backtrack
parseExpr s portTable (PrimFCall x) = parseFCall s portTable x

parseExpr s portTable (PrimLit c)= do --This will create a literal.
  n1 <- getNewId
  n2 <- getNewId
  return (Backtrack (Literal ("lit" ++ "operatorId" ++ n1) c (portLike ("newPortId" ++ n2) (sureLookup s portTable)) ()) [] [])

--verwijst naar de meegegeven VHDL naam. Kan in een latere iteratie worden weggehaald
--mogelijk dient ook hier PortLike gebruikt worden..
parseExpr s portTable (PrimName x)  = do 
  return (Backtrack (PortReference (SinglePort (parseFName x))) [] []) --indicates that we found a VHDL name, e.a. a reference to another signal.
  --parseExpr currently doesn't have the information to resolve this recursivly and therefore the PortReference datatype indicates that this should be resolved later on.

--we have not come across this type in one of our examples yet. We will implement it when we encounter it.   
parseExpr s portTable (Aggregate eas)=undefined 

--these are all the standard operators that are defined in Expr, they're all parsed in the same way:
parseExpr s portTable (And x y)   =parseBinExpr s portTable "and" x y
parseExpr s portTable (Or x y)    =parseBinExpr s portTable "or" x y
parseExpr s portTable (Xor x y)   =parseBinExpr s portTable "xor" x y
parseExpr s portTable (Nand x y)  =parseBinExpr s portTable "nand" x y
parseExpr s portTable (Nor x y)   =parseBinExpr s portTable "nor" x y
parseExpr s portTable (Xnor x y)  =parseBinExpr s portTable "xnor" x y
parseExpr s portTable (Mod x y)   =parseBinExpr s portTable "mod" x y
parseExpr s portTable (Rem x y)   =parseBinExpr s portTable "rem" x y
parseExpr s portTable (Sll x y)   =parseBinExpr s portTable "sll" x y
parseExpr s portTable (Srl x y)   =parseBinExpr s portTable "srl" x y
parseExpr s portTable (Sla x y)   =parseBinExpr s portTable "sla" x y
parseExpr s portTable (Sra x y)   =parseBinExpr s portTable "sra" x y
parseExpr s portTable (Rol x y)   =parseBinExpr s portTable "rol" x y
parseExpr s portTable (Ror x y)   =parseBinExpr s portTable "ror" x y
parseExpr s portTable (x :=: y)   =parseBinExpr s portTable "=" x y
parseExpr s portTable (x :/=: y)  =parseBinExpr s portTable "/=" x y
parseExpr s portTable (x :<: y)   =parseBinExpr s portTable "<" x y
parseExpr s portTable (x :<=: y)  =parseBinExpr s portTable "<=" x y
parseExpr s portTable (x :>: y)   =parseBinExpr s portTable ">" x y
parseExpr s portTable (x :>=: y)  =parseBinExpr s portTable ">=" x y
parseExpr s portTable (x :+: y)   =parseBinExpr s portTable "+" x y
parseExpr s portTable (x :-: y)   =parseBinExpr s portTable "-" x y
parseExpr s portTable (x :&: y)   =parseBinExpr s portTable "&" x y
parseExpr s portTable (x :*: y)   =parseBinExpr s portTable "*" x y
parseExpr s portTable (x :/: y)   =parseBinExpr s portTable "/" x y
parseExpr s portTable (x :**: y)  =parseBinExpr s portTable "**" x y

--these are all the standard unairy operators that are defined in Expr, they're all parsed in the same way:
parseExpr s portTable (Neg x)     =parseUnairyExpr s portTable "neg" x
parseExpr s portTable (Pos x)     =parseUnairyExpr s portTable "pos" x
parseExpr s portTable (Abs x)     =parseUnairyExpr s portTable "abs" x
parseExpr s portTable (Not x)     =parseUnairyExpr s portTable "not" x


--This function parses a binary expression. name is imply the name the Operator will get.
--It first parses its sub expressions recursivly. Then it creates the current operator. 
--The ports of the current Operator are creatediunder the assuption that the operator has the same Porttype on both it's inports as well as on its outport.
--We assume these assuptions are always valid for the standard operators that are defined in Expr. 
--Note that this doens't aply for Functions defined in Expr even though they're also parsed to operators. They are parsed in parseFCall
parseBinExpr:: String -> [(String,Port)] -> String -> Expr -> Expr -> EnvSession Backtrack
parseBinExpr s portTable name x y = do
  n1 <- getNewId
  n2 <- getNewId
  n3 <- getNewId
  n4 <- getNewId
  subOpX <- parseExpr s portTable x 
  subOpY <- parseExpr s portTable y
  let currOperator=Operator ("operatorId" ++ n1) name ins out ()
      in1=portLike ("newPortId" ++ n2) portType
      in2=portLike ("newPortId" ++ n3) portType
      ins=[in1,in2]
      out=portLike ("newPortId" ++ n4) portType
      parsedsubOps=[subOpX,subOpY]
      portType=sureLookup s portTable --although this is a valid Port, it may not be unique. therefore we use a portLike on it instead of using this exact port for the outport.
      
      result=connect parsedsubOps currOperator "an expression wire"
  return result

--Parses unairy expressions in the same way as the binary expressions:
parseUnairyExpr :: String -> [(String,Port)] -> String -> Expr -> EnvSession Backtrack
parseUnairyExpr s portTable name x = do
    n1 <- getNewId
    n2 <- getNewId
    n3 <- getNewId
    parsedsubOp <- parseExpr s portTable x
    let portType=sureLookup s portTable
        --although this is a valid Port, it may not be unique. therefoe we use a portLike instead of this excact port for the outport.
        ins=[portLike ("newPortId" ++ n1) portType]
        out=portLike ("newPortId" ++ n2) portType
        currOperator=Operator ("operatorId" ++ n3) name ins out ()
        result=connect [parsedsubOp] currOperator "an expression wire"
    return result

parseFCall s portTable (FCall (NSimple (Basic "to_signed"))
  [Nothing :=>: ADExpr (PrimLit x)
  ,Nothing :=>: ADExpr (PrimLit y)])
  = parseExpr s portTable (PrimLit x)
  --dit laat de to_signed functie en diens tweede argument weg, is in princiepe niet nodig aangezien dit ook later bij de GUI gedaan kan worden (nu gaat data van het type verloren), maar geeft een netter uitzient resultaat.

parseFCall s portTable (FCall functionName assocElems) = do
  n1 <- getNewId
  n2 <- getNewId
  parsedsubOps <- mapM (parseAssocElem s portTable) assocElems --recursivly parses everething attached to this function's inports.
  let inputLength= length parsedsubOps
  inportIds <- mapM (\_ -> getNewId) [1..inputLength]
  let
    fName= parseFName functionName
    newId= fName ++ "operatorId" ++ n1

    --Als in subParse ook een uitvoer zit, wordt dit 1 minder en worden andere dingen ook wat ingewikkelder, ik neem hier aan dat dit niet het geval is, omdat ik het niet weet en ik niet graag onnodig werk doe.
    inports=[portLike ("newPortId" ++ i) t|(i,t) <- zip inportIds typeInPorts] --creates inports of the appropriate type

    typeInPorts=map (outportOf.archElem) parsedsubOps --tries to deduce it's inports type by looking at the type of outport it's attached to. TODO:currently this will fail when it's attacked to a PortReference.

    currOperator= Operator newId fName inports outport ()   --functions in de VHDL are mapped to operators in our datastruct.
    typeOutPort= sureLookup s portTable
    outport=portLike ("newPortId" ++ n2) typeOutPort
    result=connect parsedsubOps currOperator "a function call wire"
    --mogelijk afhankelijk van subParse (als daar een uitvoernaam bijzit)? Zo ja, later aanpassen
  return result

parseAssocElem :: String -> [(String,Port)] -> AssocElem -> EnvSession (Backtrack)
parseAssocElem s portTable (Nothing :=>: (Open)) = return Backtrack {archElem=PortReference (SinglePort ("Nothing :=>: (Open) kan nog niet geparsd worden")),wires=[],prevArchElems=[]}
parseAssocElem s portTable (Nothing :=>: (ADExpr e)) = parseExpr s portTable e
parseAssocElem s portTable (Nothing :=>: (ADName x)) = parseExpr s portTable (PrimName x)
--Hiervoor moet x waarschijnlijk opgezocht kunnen worden en moet dus mogelijk meer informatie aan parseExpr worden meegegeven:
parseAssocElem s portTable (Just x :=>: y) = return Backtrack {archElem=PortReference (SinglePort ("Just _ :=>: _ kan nog niet geparsd worden")),wires=[],prevArchElems=[]}

--ik heb deze methode zonder het in te zien 2 keer geschreven.. zie parseVHDLName in ParseVHDLvoor z´n duplicaat...:
parseFName :: VHDLName -> String
parseFName (NSimple x)                 = parseId x
parseFName (NSelected (vhdlName :.: (SSimple id))) = (parseFName vhdlName) ++ "." ++ parseId id
parseFName (NIndexed x)                = "kan nog niet geparsd worden" ++ show (x)
parseFName (NSlice x)                  = "kan nog niet geparsd worden" ++ show (x)
parseFName (NAttribute x)              = "kan nog niet geparsd worden" ++ show (x)

parseId::  VHDLId-> Id
parseId s=fromVHDLId s


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


--------------------------------------------------------
-- Parsing Function inside Function architecture
--------------------------------------------------------

-- | The InsUnit specifies another entity with architecture. Because
-- | it does not hold the architecture the VHDLId refers to the proper
-- | VHDL Designfile. This file is parsed the same way as the current 
-- | Designfile. The Function that is return will be a Function in this
-- | Function.
parseInsUnit :: InsUnit -> EnvSession (ArchElem ())
parseInsUnit (IUEntity name) = do 
  let filename = parseVHDLName name
  vhdl <- searchVHDLsById filename
  parseDesignFile vhdl

-- | A Portmap consists of a list of AssocElems which literly describe 
-- | the wires. The wires to the inport of the Function are added to the
-- | Backtrack element and PortReferences are made. For the outpot of the 
-- | Function the proper PortReferens is return and will be used to resolve
-- | the other references.
parsePMapAspect :: [AssocElem] -> (ArchElem (), [Wire ()], [ArchElem ()])
-- Last in the list is the outport of the Function
parsePMapAspect(a@((Just start) :=>: ADExpr (PrimName (NSimple destination))):[]) = (resolve,[],[])
  where
    resolve = (PortReference (SinglePort (parseSimpleName destination)))
-- Inports to the function
parsePMapAspect (a@((Just destination) :=>: ADExpr (PrimName (NSimple start))):ass) = (resolve, wire:recursiveWires, ref:recursiveRefs)
  where
    wire = (Wire Nothing (parseSimpleName start) (parseSimpleName destination) ())
    ref = (PortReference (SinglePort (parseSimpleName start)))
    (resolve, recursiveWires, recursiveRefs) = parsePMapAspect ass

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


--------------------------------------------------------
-- Resolving Port References & Associations
--------------------------------------------------------

removeReferences :: ([Wire ()],[ArchElem ()]) -> [(ArchElem (),Backtrack2)] -> [String] -> ([Wire ()],[ArchElem ()])
removeReferences (ws,(a@(PortReference (SinglePort x)):as)) table ins
  | x `elem` ins = (fst niksVeranderd, snd niksVeranderd) --HACKED??
  | otherwise = (fst r , snd r)
    where
      r=  removeReferences ((ws  \\ [w] )  ++ (fst newReferences),(snd newReferences) ++ as ) table ins --TODO: dit geeft een oneindige lus als een element dat al bestond weer wordt toegevoegd, check of union i.p.v. concatteneren hier correct resultaat opleverd..
      (_,newReferences) = resolveAssociationNamed table ins i x --mogelijk moeten alle associaties eerder worden verholpen om i te kunnen vinden, dan krijgen we de error in findInof..
      
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
    
myGeneralizedLookup :: ArchElem () -> [(ArchElem (),Backtrack2)] -> [(PortId,Backtrack2)]
myGeneralizedLookup (PortReference (SinglePort x)) []=[]
myGeneralizedLookup (r@(PortReference (SinglePort x))) (((PortReference (SinglePort y)),result):ps)
   |(untillDot x) == (untillDot y) = (y,result) : (myGeneralizedLookup r ps)
   |otherwise                     = (myGeneralizedLookup r ps)

resolveassociation ::  LookupTable2 -> [String] -> String -> (LookupTable2,([Wire ()],[ArchElem ()]))
resolveassociation table ins i =resolveAssociationNamed table ins i i

resolveAssociationNamed :: [(ArchElem (), Backtrack2)] -> [String] -> String -> String -> (LookupTable2,([Wire ()],[ArchElem ()]))
resolveAssociationNamed table ins outName x --x is a signaalname that can be found in a PortReference
  |(allRelated == []) && (not (isIn (untillDot x))) = error $ "We kunnen " ++ x ++ " niet vinden: \n ins are:" ++ (show ins)
  |otherwise =(newTable,result)
   where
     toBeResolvedReference=(PortReference $ SinglePort x)
     allRelated :: [(PortId,Backtrack2)]
     allRelated=myGeneralizedLookup toBeResolvedReference table -- TODO: x kan iets zijn als naam.A , als dat het geval is moet op naam worden gezocht. Ook kan het zijn dat als x naam is er in de tabel bijvoorbeeld een naam.A en een naam.B staat.
     exactFound=lookup x allRelated
     Just exact=exactFound
      
     currRess | exactFound == Nothing = (map snd allRelated)  --neemt gewoon alles
              | otherwise =  [exact]
             --Als we naar iets zoeken wat we ergens al excact geparsed hebben, gaan we geen overbodige extra componenten opleveren.
             --Dus als we hebben ¨x <- naam.B, naam.B <- y¨ gaan we meteen naar het resultaat y en kijken we niet naar resultaten van bv naam.A parsen.
     removeFromTable :: LookupTable2
     removeFromTable | exactFound == Nothing = map (\(x,t) -> (PortReference (SinglePort x),t)) allRelated
                     | otherwise = [(toBeResolvedReference,exact)]

     result :: ([Wire ()],[ArchElem ()])
     result |isIn (untillDot x)            =([inWire],[])
            |otherwise                     =concatted
     isIn y= elem y ins
     inWire=Wire (Just x) x outName ()
     concatted = (concat $ fst unzipped, concat $ snd unzipped)
     unzipped = unzip checkAll

     (newTable,checkAll) = mapAccumL (resolveFoundAssociation ins outName x) table currRess

resolveFoundAssociation ins outName x table currRes
  | alGehad = (table,([newWire],[]))
  | otherwise = (resTable,result)
    where
     alGehad = seen currRes
     result |not (checkIsReference firstElem)=(newWire: (wires $ bt currRes),((archElem $ bt currRes) : (prevArchElems $ bt currRes)))
            |otherwise                     =solveRecursivly
     resTable | not (checkIsReference firstElem) = table2
              | otherwise = newTable
     thisTableEntry =(PortReference (SinglePort x),currRes)
     table2=(table \\ [thisTableEntry]) ++ [setToTrue thisTableEntry ]
     firstElem=archElem $ bt currRes
     newWire = Wire (Just x) wireStartId outName ()
     wireStartId= (untillDot (getHighest(outportOf firstElem))) ++ (fromdot x)
     --newX=getHighest(outportOf firstElem)
     PortReference (SinglePort newX) = firstElem
     solveRecursivly = ((fst recursivlyResolved) ++ (wires $ bt currRes) , (snd recursivlyResolved) ++ (prevArchElems $ bt currRes)) --signalen mogen niet rechtstreeks recursief zijn opgescheven, omdat anders hier een oneindige loop ontstaat. Dus geen rechtstreekse a<- b, b<- a of varianten hierop. recursie van signalen binnen elementen zoals registers zal hier geen probleem geven.
     (newTable,recursivlyResolved)=resolveAssociationNamed table2 ins outName newX

     setToTrue :: (ArchElem (),Backtrack2) -> (ArchElem (),Backtrack2) 
     setToTrue = (\(a,bt) -> (a,bt{seen=True}))

isInSignal :: ArchElem a -> [String] -> Bool
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


--------------------------------------------------------
-- Parsing Register elements
--------------------------------------------------------

parseBlockSm :: BlockSm -> [(String,Port)] -> EnvSession (ArchElem (), Backtrack)
parseBlockSm b@(BlockSm l _ _ _ _) portTable
  | label == "state" = parseState b portTable
  | otherwise = error $ "can't yet parse BlockSm with label " ++ label
  where label = fromVHDLId l
