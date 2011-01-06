module ParseExpr where
import Language.VHDL.AST hiding (Function)
import Datastruct
import Helper
import Data.List

parseFCall s portTable (FCall (NSimple (Basic "to_signed"))
  [Nothing :=>: ADExpr (PrimLit x)
  ,Nothing :=>: ADExpr (PrimLit y)]) n m
  = parseExpr s portTable (PrimLit x) n m 
  --dit laat de to_signed functie en diens tweede argument weg, is in princiepe niet nodig aangezien dit ook later bij de GUI gedaan kan worden (nu gaat data van het type verloren), maar geeft een netter uitzient resultaat.


--dit deel werkt niet. Dit dient nog aangepast te worden:
parseFCall s portTable (FCall functionName assocElems) n m =
     result
      {-(Function newId (Just fName) (map SinglePort inports) outport ([],[]) () 
      --aan de hand van de naam van de functie: fName, kunnen de inwendige componenten worden opgezocht wanneer nodig
      ,[Wire Nothing (associatedOutports!!i) (inports!!i) ()| i<- [0..(inputLength-1)] ] ++ concat (map get2out5 subParse)
      ,concat(map get3out5 subParse)
      ,n+inputLength
      ,m+1
      )-}
    where 
      fName= parseFName functionName
      newId= fName ++ operatorId m
      ((newN,newM),parsedsubOp)=mapAccumL (myparseAssocElem s portTable) (n+inputLength+1,m+1) assocElems

      myparseAssocElem z x c v=((get4out5 re, get5out5 re),re)
        where re=parseAssocElem z x c v
      
      --TODO mappen met doorgeven n en m : map (parseAssocElem n+inputLength m+1) s portTable assocElems
      inputLength= length parsedsubOp 
      --Als in subParse ook een uitvoer, wordt dit 1 minder en worden andere dingen ook wat ingewikkelder, ik neem hier aan dat dit niet het geval is, omdat ik het niet weet en ik niet graag onnodig werk doe.
      inports=[newPortId (i+n)|i<- [1..inputLength]]


      currOperator= Operator newId fName (map SinglePort inports) outport ()    --Function newId (Just fName) (map SinglePort inports) outport ([],[]) ()
      typeOutPort= sureLookup s portTable
      outport=portLike (newPortId n) typeOutPort
      result=connect parsedsubOp currOperator "an expression wire" 
      --mogelijk afhankelijk van subParse (als daar een uitvoernaam bijzit)? Zo ja, later aanpassen

parseAssocElem :: String -> [(String,Port)] -> (Int,Int) -> AssocElem  -> (ArchElem (),[Wire ()],[ArchElem ()],Int,Int)
parseAssocElem s portTable (n,m) (Nothing :=>: (Open))=  (PortReference (SinglePort ("Nothing :=>: (Open) kan nog niet geparsd worden")),[],[],n,m)
parseAssocElem s portTable (n,m) (Nothing :=>: (ADExpr e))= parseExpr s portTable e n m
parseAssocElem s portTable (n,m) (Nothing :=>: (ADName x))= parseExpr s portTable (PrimName x) n m
--Hiervoor moet x waarschijnlijk opgezocht kunnen worden en moet dus mogelijk meer informatie aan parseExpr worden meegegeven:
parseAssocElem s portTable (n,m) (Just x :=>: y) = (PortReference (SinglePort ("Just _ :=>: _ kan nog niet geparsd worden")),[],[],n,m)

--ik heb deze methode zonder het in te zien 2 keer geschreven.. zie parseVHDLName in ParseVHDLvoor z´n duplicaat...:
parseFName :: VHDLName -> String
parseFName (NSimple x)                 = parseId x
parseFName (NSelected (vhdlName :.: (SSimple id))) = (parseFName vhdlName) ++ "." ++ parseId id
parseFName (NIndexed x)                = "kan nog niet geparsd worden" ++ show (x)
parseFName (NSlice x)                  = "kan nog niet geparsd worden" ++ show (x)
parseFName (NAttribute x)              = "kan nog niet geparsd worden" ++ show (x)

{-
mooiere versie?:
newParseExpr :: [ConSigAssignSm] -> (Int, Int) -> Expr-> ((Int,Int),(Port ,[Wire ()],[ArchElem ()]))
newparseExpr consSms ints (PrimName x)= 
               parseFName x
                   where 
				         signalName=parseFName x
						 currAssign=lookup consSms signalName
 
-}

parseExpr :: String -> [(String,Port)] -> Expr-> Int -> Int -> (ArchElem (),[Wire ()],[ArchElem ()],Int,Int)

parseExpr s portTable (PrimFCall x) n m = parseFCall s portTable x n m

parseExpr s portTable (PrimLit c) n m = ((Literal ("lit" ++ operatorId m) c (portLike (newPortId n) (sureLookup s portTable)) ()),[],[],n+1,m+1)
parseExpr s portTable (PrimName x) n m= (PortReference (SinglePort (parseFName x)),[],[],n,m) --verwijst naar de meegegeven VHDL naam. Kan in een latere iteratie worden weggehaald
                                      --mogelijk dient ook hier PortLike gebruikt worden..
parseExpr s portTable (Aggregate eas) n m=undefined --we have not come across this type in one of our examples yet. We will implement it when we encounter it.

--is er een manier om n en m hier weg ta halen uit de code zoals bij Amanda?
--dit blijkt niet te kunnen -_-  ... :(
parseExpr s portTable (And x y) n m   =parseBinExpr s portTable "and" x y n m
parseExpr s portTable (Or x y) n m    =parseBinExpr s portTable "or" x y n m
parseExpr s portTable (Xor x y) n m   =parseBinExpr s portTable "xor" x y n m
parseExpr s portTable (Nand x y) n m  =parseBinExpr s portTable "nand" x y n m
parseExpr s portTable (Nor x y) n m   =parseBinExpr s portTable "nor" x y n m
parseExpr s portTable (Xnor x y) n m  =parseBinExpr s portTable "xnor" x y n m
parseExpr s portTable (Mod x y) n m  =parseBinExpr s portTable "mod" x y n m
parseExpr s portTable (Rem x y) n m  =parseBinExpr s portTable "rem" x y n m
parseExpr s portTable (Sll x y) n m  =parseBinExpr s portTable "sll" x y n m
parseExpr s portTable (Srl x y) n m  =parseBinExpr s portTable "srl" x y n m
parseExpr s portTable (Sla x y) n m  =parseBinExpr s portTable "sla" x y n m
parseExpr s portTable (Sra x y) n m  =parseBinExpr s portTable "sra" x y n m
parseExpr s portTable (Rol x y) n m  =parseBinExpr s portTable "rol" x y n m
parseExpr s portTable (Ror x y) n m  =parseBinExpr s portTable "ror" x y n m

parseExpr s portTable (x :=: y) n m   =parseBinExpr s portTable "=" x y  n m
parseExpr s portTable (x :/=: y) n m   =parseBinExpr s portTable "/=" x y n m
parseExpr s portTable (x :<: y) n m   =parseBinExpr s portTable "<" x y n m
parseExpr s portTable (x :<=: y) n m   =parseBinExpr s portTable "<=" x y n m
parseExpr s portTable (x :>: y) n m   =parseBinExpr s portTable ">" x y n m
parseExpr s portTable (x :>=: y) n m   =parseBinExpr s portTable ">=" x y n m
parseExpr s portTable (x :+: y) n m   =parseBinExpr s portTable "+" x y n m
parseExpr s portTable (x :-: y) n m   =parseBinExpr s portTable "-" x y n m
parseExpr s portTable (x :&: y) n m   =parseBinExpr s portTable "&" x y n m
parseExpr s portTable (x :*: y) n m   =parseBinExpr s portTable "*" x y n m
parseExpr s portTable (x :/: y) n m   =parseBinExpr s portTable "/" x y n m
parseExpr s portTable (x :**: y) n m  =parseBinExpr s portTable "**" x y n m



parseExpr s portTable (Neg x) n m     =parseUnairyExpr s portTable "neg" x n m
parseExpr s portTable (Pos x) n m     =parseUnairyExpr s portTable "pos" x n m
parseExpr s portTable (Abs x) n m     =parseUnairyExpr s portTable "abs" x n m
parseExpr s portTable (Not x) n m     =parseUnairyExpr s portTable "not" x n m


parseBinExpr:: String -> [(String,Port)] -> String -> Expr -> Expr -> Int -> Int -> (ArchElem (),[Wire ()],[ArchElem ()],Int,Int)
parseBinExpr s portTable name x y n m = result
   where
    currOperator=Operator (operatorId m) name ins out ()
    in1=portLike (newPortId n) portType
    in2=portLike (newPortId (n+1)) portType
    ins=[in1,in2]
    out=portLike (newPortId (n+2)) portType

    subOpX=parseExpr s portTable x (n+3) (m+1)
    subOpY=parseExpr s portTable y (get4out5 subOpX) (get5out5 subOpX)
    parsedsubOps=[subOpX,subOpY]

    portType=sureLookup s portTable --although this is a valid Port, it may not be unique. therefoe we use a portLike instead of this excact port for the outport.
    result=connect parsedsubOps currOperator "an expression wire" --Ik gebruik nu ook hier connect i.p.v. de dingen expliciet uit te schrijven omdat dit algemener is en meer compositioneel is, wat in het onderhoudt mogelijk handig is.

parseUnairyExpr :: String -> [(String,Port)] -> String -> Expr -> Int -> Int -> (ArchElem (),[Wire ()],[ArchElem ()],Int,Int)
parseUnairyExpr s portTable name x n m=result 
  where
    currOperator=Operator (operatorId m) name ins out ()
    ins=[portLike (newPortId n) portType]
    out=portLike (newPortId (n+1)) portType

    parsedsubOp=[parseExpr s portTable x (n+2) (m+1)]

    portType=sureLookup s portTable --although this is a valid Port, it may not be unique. therefoe we use a portLike instead of this excact port for the outport.
    result=connect parsedsubOp currOperator "an expression wire" --Ik gebruik nu ook hier connect i.p.v. de dingen expliciet uit te schrijven omdat dit algemener is en meer compositioneel is, wat in het onderhoudt mogelijk handig is.

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
