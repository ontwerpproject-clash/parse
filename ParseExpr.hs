module ParseExpr where
import Language.VHDL.AST hiding (Function)
import Datastruct
import Helper

addSubportToBottom :: Port -> Port -> Port
addSubportToBottom (SinglePort x) y = MultiPort x [y]
addSubportToBottom (MultiPort x [z]) y = MultiPort x [addSubportToBottom z y]

--tijdelijke naamparseer voor in expressies:
parseVHDLName :: VHDLName -> Port
parseVHDLName (NSimple x)                 = SinglePort (parseId x)
--Bij een geselecteerde naam wordt alleen de huidige subpoortstructuur teruggegeven:
--Als bij z=(a,b,c) de naam z.b tegengekomen wordt, leverd dit bv alleen MultiPort z [SinglePort b] op.
--parseVHDLName (NSelected (vhdlName :.: id)) = addSubportToBottom ((parseVHDLName vhdlName) (parseId id))
parseVHDLName (NIndexed x)                = SinglePort ("kan nog niet geparsd worden" ++ show (x))
parseVHDLName (NSlice x)                  = SinglePort ("kan nog niet geparsd worden" ++ show (x))
parseVHDLName (NAttribute x)              = SinglePort ("kan nog niet geparsd worden" ++ show (x))

parseFCall (FCall (NSimple (Basic "to_signed"))
  [Nothing :=>: ADExpr (PrimLit x)
  ,Nothing :=>: ADExpr (PrimLit y)]) n m
  = parseExpr (PrimLit x) n m 
  --dit laat de to_signed functie en diens tweede argument weg, is in princiepe niet nodig aangezien dit ook later bij de GUI gedaan kan worden (nu gaat data van het type verloren), maar geeft een netter uitzient resultaat.

parseFCall (FCall functionName assocElems) n m =
      (Function (fName ++ operatorId m) (Just fName) (map SinglePort inports) outport ([],[]) () 
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
      --TODO mappen met doorgeven n en m : map (parseAssocElem n+inputLength m+1) assocElems
      inputLength= length subParse 
      --Als in subParse ook een uitvoer, wordt dit 1 minder en worden andere dingen ook wat ingewikkelder, ik neem hier aan dat dit niet het geval is, omdat ik het niet weet en ik niet graag onnodig werk doe.
      inports=[newPortId (i+n)|i<- [0..(inputLength-1)]]
      associatedOutports=map outOf subParse
      outport=SinglePort $ newPortId (n) 
      --mogelijk afhankelijk van subParse (als daar een uitvoernaam bijzit)? Zo ja, later aanpassen

parseAssocElem :: AssocElem -> Int -> Int -> (ArchElem (),[Wire ()],[ArchElem ()],Int,Int)
parseAssocElem (Nothing :=>: (Open)) n m=  (PortReference (SinglePort ("Nothing :=>: (Open) kan nog niet geparsd worden")),[],[],n,m)
parseAssocElem (Nothing :=>: (ADExpr e)) n m= parseExpr e n m
parseAssocElem (Nothing :=>: (ADName x)) n m= parseExpr (PrimName x) n m
--Hiervoor moet x waarschijnlijk opgezocht kunnen worden en moet dus mogelijk meer informatie aan parseExpr worden meegegeven:
parseAssocElem (Just x :=>: y) n m = (PortReference (SinglePort ("Just _ :=>: _ kan nog niet geparsd worden")),[],[],n,m)

parseFName :: VHDLName -> String
parseFName (NSimple x)                 = parseId x
parseFName (NSelected (vhdlName :.: (SSimple id))) = (parseFName vhdlName) ++ "." ++ parseId id
parseFName (NIndexed x)                = "kan nog niet geparsd worden" ++ show (x)
parseFName (NSlice x)                  = "kan nog niet geparsd worden" ++ show (x)
parseFName (NAttribute x)              ="kan nog niet geparsd worden" ++ show (x)

parseExpr ::Expr-> Int -> Int -> (ArchElem (),[Wire ()],[ArchElem ()],Int,Int)

parseExpr (PrimFCall x) n m = parseFCall x n m

parseExpr (PrimLit c) n m = ((Literal ("lit" ++ operatorId m) c (SinglePort (newPortId n)) ()),[],[],n+1,m+1)
parseExpr (PrimName x) n m= (PortReference (SinglePort (parseFName x)),[],[],n,m) --verwijst naar de meegegeven VHDL naam. Kan in een latere iteratie worden weggehaald
                           --mogelijk dient dit PortReference (parseVHDLName x) te zijn.
parseExpr (Aggregate eas) n m=undefined

--is er een manier om n en m hier wer ta halen uit de code zoals bij Amanda?
parseExpr (And x y) n m   =parseBinExpr "and" x y n m
parseExpr (Or x y) n m    =parseBinExpr "or" x y n m
parseExpr (Xor x y) n m   =parseBinExpr "xor" x y n m
parseExpr (Nand x y) n m  =parseBinExpr "nand" x y n m
parseExpr (Nor x y) n m   =parseBinExpr "nor" x y n m
parseExpr (Xnor x y) n m  =parseBinExpr "xnor" x y n m
parseExpr (x :=: y) n m   =parseBinExpr "=" x y  n m
parseExpr (x :/=: y) n m  =parseBinExpr "/=" x y n m
parseExpr (x :<: y) n m   =parseBinExpr "<" x y n m
parseExpr (x :<=: y) n m  =parseBinExpr "<=" x y n m
parseExpr (x :>: y) n m   =parseBinExpr ">" x y n m
parseExpr (x :>=: y) n m  =parseBinExpr ">=" x y n m
parseExpr (x :+: y) n m   =parseBinExpr "+" x y n m

parseExpr (Neg x) n m     =parseUnairyExpr "neg" x n m
parseExpr (Pos x) n m     =parseUnairyExpr "pos" x n m

--andere expressies gaan soortgelijk..
--Dus soortgelijk kan gedaan worden voor Adding Operators,Multiplying Operators en Shift Operators en Miscellaneous Operators

parseBinExpr:: String -> Expr -> Expr -> Int -> Int -> (ArchElem (),[Wire ()],[ArchElem ()],Int,Int)
parseBinExpr name x y n m = 
    (Operator  (operatorId m) name [in1,in2] (SinglePort (newPortId (n+2))) () ,
    [Wire (Just "bool") (outOf (get1out5 subOpX)) in1 ()
    ,Wire (Just "bool") (outOf (get1out5 subOpY)) in2 ()] ++ (get2out5 subOpX) ++ (get2out5 subOpY),
    [get1out5 subOpX,get1out5 subOpY] ++ (get3out5 subOpX) ++ (get3out5 subOpY), get4out5 subOpY, get5out5 subOpY)
  where
    subOpX=parseExpr x (n+3) (m+1)
    subOpY=parseExpr y (get4out5 subOpX) (get5out5 subOpX)
    in1=newPortId n 
    in2=newPortId (n+1)

parseUnairyExpr :: String -> Expr -> Int -> Int -> (ArchElem (),[Wire ()],[ArchElem ()],Int,Int)
parseUnairyExpr name x n m=
    (Operator (operatorId m) name [in1] (SinglePort (newPortId (n+1))) () ,
    [Wire (Just "num") (outOf (get1out5 subOpX)) in1 ()] ++ (get2out5 subOpX),
    [get1out5 subOpX] ++ (get3out5 subOpX), get4out5 subOpX, get5out5 subOpX)
  where
    subOpX=parseExpr x (n+2) (m+1)
    in1=newPortId n

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

operatorId::Int -> String
operatorId m= "operatorId" ++ show m

newPortId m= "newPortId" ++ show m

parseId::  VHDLId-> Id
parseId s=fromVHDLId s