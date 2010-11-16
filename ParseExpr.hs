module ParseExpr where
import Language.VHDL.AST hiding (Function)
import Datastruct


-- == Helper functions
addSubportToBottom :: Port -> Port -> Port
addSubportToBottom (SinglePort x) y = MultiPort x [y]
addSubportToBottom (MultiPort x [z]) y = MultiPort x [addSubportToBottom z y]




-- =========================


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
             ,Nothing :=>: ADExpr (PrimLit y)]) n m= parseExpr (PrimLit x) n m --dit laat de to_signed functie en diens tweede argument weg, is in princiepe niet nodig aangezien dit ook later bij de GUI gedaan kan worden (nu gaat data van het type verloren), maar geeft een netter uitzient resultaat.

parseFCall (FCall functionName assocElems) n m
                               = (Function (fName ++ operatorId m) (Just fName) (map SinglePort inports) outport ([],[]) () --aan de hand van de naam van de functie: fName, kunnen de inwendige componenten worden opgezocht wanneer nodig
                                 ,[Wire Nothing (associatedOutports!!i) (inports!!i) ()| i<- [0..(inputLength-1)] ] ++ concat (map snd5 subParse)
                                 ,concat(map trd5 subParse)
                                 ,n+inputLength
                                 ,m+1
                                 )
                                  where fName= parseFName functionName
                                        newId= fName ++ operatorId m
                                        subParse=[] --TODO mappen met doorgeven n en m : map (parseAssocElem n+inputLength m+1) assocElems
                                        inputLength= length subParse --Als in subParse ook een uitvoer, wordt dit 1 minder en worden andere dingen ook wat ingewikkelder, ik neem hier aan dat dit niet het geval is, omdat ik het niet weet en ik niet graag onnodig werk doe.
                                        inports=[newPortId (i+n)|i<- [0..(inputLength-1)]]
                                        associatedOutports=map outOf subParse
                                        outport=SinglePort $ newPortId (n) --mogelijk afhankelijk van subParse (als daar een uitvoernaam bijzit)? Zo ja, later aanpassen

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

parseExpr (PrimLit c) n m = ((Literal (operatorId "lit" ++ show m) c (SinglePort (newPortId n)) ()),[],[],n+1,m+1)
parseExpr (PrimName x) n m= (PortReference (SinglePort (parseFName x)),[],[],n,m) --verwijst naar de meegegeven VHDL naam. Kan in een latere iteratie worden weggehaald
                           --mogelijk dient dit PortReference (parseVHDLName x) te zijn.
parseExpr (And x y) n m=(Operator  (operatorId m) "and" [in1,in2] (SinglePort (newPortId (n+2))) () ,
                        [Wire (Just "bool") (outOf (fst5 subOpX)) in1 ()
                        ,Wire (Just "bool") (outOf (fst5 subOpY)) in2 ()] ++ (snd5 subOpX) ++ (snd5 subOpY),
                         [fst5 subOpX,fst5 subOpY] ++ (trd5 subOpX) ++ (trd5 subOpY), getN subOpY, getM subOpY)
                         where subOpX=parseExpr x (n+3) (m+1)
                               subOpY=parseExpr y (getN subOpX) (getM subOpX)
                               in1=newPortId n 
                               in2=newPortId (n+1)


parseExpr (Or x y) n m=(Operator  (operatorId m) "or" [in1,in2] (SinglePort (newPortId (n+2))) () ,
                        [Wire (Just "bool") (outOf (fst5 subOpX)) in1 ()
                        ,Wire (Just "bool") (outOf (fst5 subOpY)) in2 ()] ++ (snd5 subOpX) ++ (snd5 subOpY),
                        [fst5 subOpX,fst5 subOpY] ++ (trd5 subOpX) ++ (trd5 subOpY), getN subOpY, getM subOpY)
                         where subOpX=parseExpr x (n+3) (m+1)
                               subOpY=parseExpr y (getN subOpX) (getM subOpX)
                               in1=newPortId n 
                               in2=newPortId (n+1)

parseExpr (Xor x y) n m=(Operator  (operatorId m) "xor" [in1,in2] (SinglePort (newPortId (n+2))) () ,
                        [Wire (Just "bool") (outOf (fst5 subOpX)) in1 ()
                        ,Wire (Just "bool") (outOf (fst5 subOpY)) in2 ()] ++ (snd5 subOpX) ++ (snd5 subOpY),
                        [fst5 subOpX,fst5 subOpY] ++ (trd5 subOpX) ++ (trd5 subOpY), getN subOpY, getM subOpY)
                         where subOpX=parseExpr x (n+3) (m+1)
                               subOpY=parseExpr y (getN subOpX) (getM subOpX)
                               in1=newPortId n 
                               in2=newPortId (n+1)

parseExpr (Nand x y) n m=(Operator  (operatorId m) "Nand" [in1,in2] (SinglePort (newPortId (n+2))) () ,
                        [Wire (Just "bool") (outOf (fst5 subOpX)) in1 ()
                        ,Wire (Just "bool") (outOf (fst5 subOpY)) in2 ()] ++ (snd5 subOpX) ++ (snd5 subOpY),
                        [fst5 subOpX,fst5 subOpY] ++ (trd5 subOpX) ++ (trd5 subOpY), getN subOpY, getM subOpY)
                         where subOpX=parseExpr x (n+3) (m+1)
                               subOpY=parseExpr y (getN subOpX) (getM subOpX)
                               in1=newPortId n 
                               in2=newPortId (n+1)


parseExpr (Nor x y) n m=(Operator  (operatorId m) "nNor" [in1,in2] (SinglePort (newPortId (n+2))) () ,
                        [Wire (Just "bool") (outOf (fst5 subOpX)) in1 ()
                        ,Wire (Just "bool") (outOf (fst5 subOpY)) in2 ()] ++ (snd5 subOpX) ++ (snd5 subOpY),
                        [fst5 subOpX,fst5 subOpY] ++ (trd5 subOpX) ++ (trd5 subOpY), getN subOpY, getM subOpY)
                         where subOpX=parseExpr x (n+3) (m+1)
                               subOpY=parseExpr y (getN subOpX) (getM subOpX)
                               in1=newPortId n 
                               in2=newPortId (n+1)


parseExpr (Xnor x y) n m=(Operator  (operatorId m) "xnor" [in1,in2] (SinglePort (newPortId (n+2))) () ,
                        [Wire (Just "bool") (outOf (fst5 subOpX)) in1 ()
                        ,Wire (Just "bool") (outOf (fst5 subOpY)) in2 ()] ++ (snd5 subOpX) ++ (snd5 subOpY),
                        [fst5 subOpX,fst5 subOpY] ++ (trd5 subOpX) ++ (trd5 subOpY), getN subOpY, getM subOpY)
                         where subOpX=parseExpr x (n+3) (m+1)
                               subOpY=parseExpr y (getN subOpX) (getM subOpX)
                               in1=newPortId n 
                               in2=newPortId (n+1)

parseExpr (x :=: y) n m=(Operator  (operatorId m) "=" [in1,in2] (SinglePort (newPortId (n+2))) () ,
                        [Wire (Just "num") (outOf (fst5 subOpX)) in1 ()
                        ,Wire (Just "num") (outOf (fst5 subOpY)) in2 ()] ++ (snd5 subOpX) ++ (snd5 subOpY),
                        [fst5 subOpX,fst5 subOpY] ++ (trd5 subOpX) ++ (trd5 subOpY), getN subOpY, getM subOpY)
                         where subOpX=parseExpr x (n+3) (m+1)
                               subOpY=parseExpr y (getN subOpX) (getM subOpX)
                               in1=newPortId n 
                               in2=newPortId (n+1)

parseExpr (x :/=: y) n m=(Operator  (operatorId m) "/=" [in1,in2] (SinglePort (newPortId (n+2))) () ,
                        [Wire (Just "num") (outOf (fst5 subOpX)) in1 ()
                        ,Wire (Just "num") (outOf (fst5 subOpY)) in2 ()] ++ (snd5 subOpX) ++ (snd5 subOpY),
                        [fst5 subOpX,fst5 subOpY] ++ (trd5 subOpX) ++ (trd5 subOpY), getN subOpY, getM subOpY)
                         where subOpX=parseExpr x (n+3) (m+1)
                               subOpY=parseExpr y (getN subOpX) (getM subOpX)
                               in1=newPortId n 
                               in2=newPortId (n+1)

parseExpr (x :<: y) n m=(Operator  (operatorId m) "<" [in1,in2] (SinglePort (newPortId (n+2))) () ,
                        [Wire (Just "bool") (outOf (fst5 subOpX)) in1 ()
                        ,Wire (Just "bool") (outOf (fst5 subOpY)) in2 ()] ++ (snd5 subOpX) ++ (snd5 subOpY),
                        [fst5 subOpX,fst5 subOpY] ++ (trd5 subOpX) ++ (trd5 subOpY), getN subOpY, getM subOpY)
                         where subOpX=parseExpr x (n+3) (m+1)
                               subOpY=parseExpr y (getN subOpX) (getM subOpX)
                               in1=newPortId n 
                               in2=newPortId (n+1)

parseExpr (x :<=: y) n m=(Operator  (operatorId m) "<=" [in1,in2] (SinglePort (newPortId (n+2))) () ,
                        [Wire (Just "num") (outOf (fst5 subOpX)) in1 ()
                        ,Wire (Just "num") (outOf (fst5 subOpY)) in2 ()] ++ (snd5 subOpX) ++ (snd5 subOpY),
                        [fst5 subOpX,fst5 subOpY] ++ (trd5 subOpX) ++ (trd5 subOpY), getN subOpY, getM subOpY)
                         where subOpX=parseExpr x (n+3) (m+1)
                               subOpY=parseExpr y (getN subOpX) (getM subOpX)
                               in1=newPortId n 
                               in2=newPortId (n+1)

parseExpr (x :>: y) n m=(Operator  (operatorId m) ">" [in1,in2] (SinglePort (newPortId (n+2))) () ,
                        [Wire (Just "num") (outOf (fst5 subOpX)) in1 ()
                        ,Wire (Just "num") (outOf (fst5 subOpY)) in2 ()] ++ (snd5 subOpX) ++ (snd5 subOpY),
                        [fst5 subOpX,fst5 subOpY] ++ (trd5 subOpX) ++ (trd5 subOpY), getN subOpY, getM subOpY)
                         where subOpX=parseExpr x (n+3) (m+1)
                               subOpY=parseExpr y (getN subOpX) (getM subOpX)
                               in1=newPortId n 
                               in2=newPortId (n+1)

parseExpr (x :>=: y) n m=(Operator  (operatorId m) ">=" [in1,in2] (SinglePort (newPortId (n+1))) () ,
                        [Wire (Just "num") (outOf (fst5 subOpX)) in1 ()
                        ,Wire (Just "num") (outOf (fst5 subOpY)) in2 ()] ++ (snd5 subOpX) ++ (snd5 subOpY),
                        [fst5 subOpX,fst5 subOpY] ++ (trd5 subOpX) ++ (trd5 subOpY), getN subOpY, getM subOpY)
                         where subOpX=parseExpr x (n+2) (m+1)
                               subOpY=parseExpr y (getN subOpX) (getM subOpX)
                               in1=newPortId n 
                               in2=newPortId (n+1)

parseExpr (x :+: y) n m=(Operator  (operatorId m) "+" [in1,in2] (SinglePort (newPortId (n+1))) () ,
                        [Wire (Just "num") (outOf (fst5 subOpX)) in1 ()
                        ,Wire (Just "num") (outOf (fst5 subOpY)) in2 ()] ++ (snd5 subOpX) ++ (snd5 subOpY),
                        [fst5 subOpX,fst5 subOpY] ++ (trd5 subOpX) ++ (trd5 subOpY), getN subOpY, getM subOpY)
                         where subOpX=parseExpr x (n+2) (m+1)
                               subOpY=parseExpr y (getN subOpX) (getM subOpX)
                               in1=newPortId n 
                               in2=newPortId (n+1)                               

parseExpr (Neg x) n m=(Operator  (operatorId m) "neg" [in1] (SinglePort (newPortId (n+1))) () ,
                        [Wire (Just "num") (outOf (fst5 subOpX)) in1 ()] ++ (snd5 subOpX),
                        [fst5 subOpX] ++ (trd5 subOpX), getN subOpX, getM subOpX)
                         where subOpX=parseExpr x (n+2) (m+1)
                               in1=newPortId n 

parseExpr (Pos x) n m=(Operator  (operatorId m) "pos" [in1] (SinglePort (newPortId (n+2))) () ,
                        [Wire (Just "num") (outOf (fst5 subOpX)) in1 ()] ++ (snd5 subOpX),
                        [fst5 subOpX] ++ (trd5 subOpX), getN subOpX, getM subOpX)
                         where subOpX=parseExpr x (n+3) (m+1)
                               in1=newPortId n 


-- soortgelijk kan gedaan worden voor Adding Operators,Multiplying Operators en Shift Operators en Miscellaneous Operators
{-
parseExpr (Aggregate eas) n m=Function (operatorId m) Nothing [] out (map trd5 subElems,map snd5 subElems) ()
                              where out=Tuple (newPortId n) (map outOf (fst5 (subElems))
                                    subElems=map parseElemAssoc eas
                                    parseElemAssoc (ElemAssoc Others e)=[parseExpr e (n+1) (m+1)]
                                    parseElemAssoc (ElemAssoc (ChoiceE e1) e2)=parseExpr (Dummy e1 e2) (n+1) (m+1)

parseExpr (Dummy x y) n m=(Operator  (operatorId m) "??" [in1,in2] (SinglePort (newPortId (n+2))) () ,
                        [Wire (Just "bool") (outOf (fst5 subOpX)) in1 ()
                        ,Wire (Just "bool") (outOf (fst5 subOpY)) in2 ()] ++ (snd5 subOpX) ++ (snd5 subOpY),
                        [fst5 subOpX,fst5 subOpY] ++ (trd5 subOpX) ++ (trd5 subOpY), getN subOpY, getM subOpY)
                         where subOpX=parseExpr x (n+3) (m+1)
                               subOpY=parseExpr y (getN subOpX) (getM subOpX)
                               in1=newPortId n 
                               in2=newPortId (n+1)


-}

-- gekopieerd ui parseDatastruc
fst5 (q,_,_,_,_)=q
snd5 (_,w,_,_,_)=w
trd5 (_,_,e,_,_)=e

getN (o,ws,ops,n,m)=n
getM (o,ws,ops,n,m)=m


outOf::ArchElem a-> PortId
outOf (Operator q w es r t)=extract (r)
                            where extract (SinglePort x)=x
outOf (Literal q v o a)=extract (o)
                        where extract (SinglePort x)=x
outOf (PortReference p)=extract (p)
                        where extract (SinglePort x)=x
                              extract (MultiPort x [y])= extract y  --dit werkt voor de huidige manier van selected names parsen


operatorId::num -> String
operatorId m= "A string"

newPortId m= "A string"


parseId::  VHDLId-> Id
parseId s=fromVHDLId s

