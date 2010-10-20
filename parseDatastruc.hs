import Data.Char (toLower)
import Text.Regex.Posix

import Error



{-
aantekeningen; taal VHDL AST:

||http://hackage.haskell.org/packages/archive/vhdl/0.1.2.1/doc/html/Language-VHDL-AST.html#t:PackageBody 

||fromVHDLId gebruiken om VHDLId om te zetten in string

data DesignFile =DesignFile [ContextItem] [LibraryUnit]

data ContextItem = Library VHDLId | Use SelectedName

data VHDLId = Basic String | Extended String




data LibraryUnit = LUEntity EntityDec      | 
                   LUArch ArchBody         | 
                   LUPackageDec PackageDec |
                   LUPackageBody PackageBody

data EntityDec = EntityDec VHDLId [IfaceSigDec]
data ArchBody = ArchBody VHDLId VHDLName [BlockDecItem] [ConcSm]
data PackageDec = PackageDec VHDLId [PackageDecItem]
data PackageDecItem = PDITD TypeDec | PDISD SubtypeDec | PDISS SubProgSpec
data PackageBody = PackageBody VHDLId [PackageBodyDecItem]
type PackageBodyDecItem = SubProgBody

-- Only subprogram bodies and signal declarations are allowed
data BlockDecItem = BDISPB SubProgBody | BDISD SigDec

-- operator_names are not allowed 
data VHDLName = NSimple SimpleName     | 
                NSelected SelectedName | 
                NIndexed IndexedName   |
                NSlice SliceName       |
                NAttribute AttribName

data IfaceSigDec = IfaceSigDec VHDLId Mode TypeMark

data Mode = In | Out

data SubProgBody = SubProgBody SubProgSpec [SubProgDecItem] [SeqSm]

data SubProgSpec = Function VHDLId [IfaceVarDec] TypeMark 

| interface_variable_declaration
data IfaceVarDec = IfaceVarDec VHDLId TypeMark


TypeMark = SimpleName
SimpleName = VHDLId
SelectedName = Prefix :.: Suffix
IndexedName = IndexedName Prefix [Expr]
Prefix = VHDLName
Suffix = SSimple SimpleName | All
SliceName = SliceName Prefix DiscreteRange
AttribName = AttribName Prefix VHDLName (Maybe Expr)

-}



operatorId::num -> String
operatorId m= "A string"

newPortId m= "A string"

myfst (q,w,e,r,t)=q
mysnd (q,w,e,r,t)=w
trd (q,w,e,r,t)=e


--hoe schrijf ik num in de type defenitie zonder dat deze fouten geeft?:
--parseExpr ::Expr-> num -> num -> (ArchElem (),[Wire ()],[ArchElem ()],num,num)


parseExpr (PrimLit c) n m = ((Literal (operatorId m) c (Normal (newPortId n)) ()),[],[],n+1,m+1)
parseExpr (PrimName x) n m=undefined

parseExpr (And x y) n m=(Operator  (operatorId m) "and" [in1,in2] (Normal (newPortId (n+2))) () ,
                        [Wire (Just "bool") (outOf (myfst subOpX)) in1 ()
                        ,Wire (Just "bool") (outOf (myfst subOpY)) in2 ()] ++ (mysnd subOpX) ++ (mysnd subOpY),
                         [myfst subOpX,myfst subOpY] ++ (trd subOpX) ++ (trd subOpY), getN subOpY, getM subOpY)
                         where subOpX=parseExpr x (n+3) (m+1)
                               subOpY=parseExpr y (getN subOpX) (getM subOpX)
                               in1=newPortId n 
                               in2=newPortId (n+1)


parseExpr (Or x y) n m=(Operator  (operatorId m) "or" [in1,in2] (Normal (newPortId (n+2))) () ,
                        [Wire (Just "bool") (outOf (myfst subOpX)) in1 ()
                        ,Wire (Just "bool") (outOf (myfst subOpY)) in2 ()] ++ (mysnd subOpX) ++ (mysnd subOpY),
                        [myfst subOpX,myfst subOpY] ++ (trd subOpX) ++ (trd subOpY), getN subOpY, getM subOpY)
                         where subOpX=parseExpr x (n+3) (m+1)
                               subOpY=parseExpr y (getN subOpX) (getM subOpX)
                               in1=newPortId n 
                               in2=newPortId (n+1)

parseExpr (Xor x y) n m=(Operator  (operatorId m) "xor" [in1,in2] (Normal (newPortId (n+2))) () ,
                        [Wire (Just "bool") (outOf (myfst subOpX)) in1 ()
                        ,Wire (Just "bool") (outOf (myfst subOpY)) in2 ()] ++ (mysnd subOpX) ++ (mysnd subOpY),
                        [myfst subOpX,myfst subOpY] ++ (trd subOpX) ++ (trd subOpY), getN subOpY, getM subOpY)
                         where subOpX=parseExpr x (n+3) (m+1)
                               subOpY=parseExpr y (getN subOpX) (getM subOpX)
                               in1=newPortId n 
                               in2=newPortId (n+1)

parseExpr (Nand x y) n m=(Operator  (operatorId m) "Nand" [in1,in2] (Normal (newPortId (n+2))) () ,
                        [Wire (Just "bool") (outOf (myfst subOpX)) in1 ()
                        ,Wire (Just "bool") (outOf (myfst subOpY)) in2 ()] ++ (mysnd subOpX) ++ (mysnd subOpY),
                        [myfst subOpX,myfst subOpY] ++ (trd subOpX) ++ (trd subOpY), getN subOpY, getM subOpY)
                         where subOpX=parseExpr x (n+3) (m+1)
                               subOpY=parseExpr y (getN subOpX) (getM subOpX)
                               in1=newPortId n 
                               in2=newPortId (n+1)


parseExpr (Nor x y) n m=(Operator  (operatorId m) "nNor" [in1,in2] (Normal (newPortId (n+2))) () ,
                        [Wire (Just "bool") (outOf (myfst subOpX)) in1 ()
                        ,Wire (Just "bool") (outOf (myfst subOpY)) in2 ()] ++ (mysnd subOpX) ++ (mysnd subOpY),
                        [myfst subOpX,myfst subOpY] ++ (trd subOpX) ++ (trd subOpY), getN subOpY, getM subOpY)
                         where subOpX=parseExpr x (n+3) (m+1)
                               subOpY=parseExpr y (getN subOpX) (getM subOpX)
                               in1=newPortId n 
                               in2=newPortId (n+1)


parseExpr (Xnor x y) n m=(Operator  (operatorId m) "xnor" [in1,in2] (Normal (newPortId (n+2))) () ,
                        [Wire (Just "bool") (outOf (myfst subOpX)) in1 ()
                        ,Wire (Just "bool") (outOf (myfst subOpY)) in2 ()] ++ (mysnd subOpX) ++ (mysnd subOpY),
                        [myfst subOpX,myfst subOpY] ++ (trd subOpX) ++ (trd subOpY), getN subOpY, getM subOpY)
                         where subOpX=parseExpr x (n+3) (m+1)
                               subOpY=parseExpr y (getN subOpX) (getM subOpX)
                               in1=newPortId n 
                               in2=newPortId (n+1)

parseExpr (x :=: y) n m=(Operator  (operatorId m) "=" [in1,in2] (Normal (newPortId (n+2))) () ,
                        [Wire (Just "bool") (outOf (myfst subOpX)) in1 ()
                        ,Wire (Just "bool") (outOf (myfst subOpY)) in2 ()] ++ (mysnd subOpX) ++ (mysnd subOpY),
                        [myfst subOpX,myfst subOpY] ++ (trd subOpX) ++ (trd subOpY), getN subOpY, getM subOpY)
                         where subOpX=parseExpr x (n+3) (m+1)
                               subOpY=parseExpr y (getN subOpX) (getM subOpX)
                               in1=newPortId n 
                               in2=newPortId (n+1)

parseExpr (x :/=: y) n m=(Operator  (operatorId m) "/=" [in1,in2] (Normal (newPortId (n+2))) () ,
                        [Wire (Just "bool") (outOf (myfst subOpX)) in1 ()
                        ,Wire (Just "bool") (outOf (myfst subOpY)) in2 ()] ++ (mysnd subOpX) ++ (mysnd subOpY),
                        [myfst subOpX,myfst subOpY] ++ (trd subOpX) ++ (trd subOpY), getN subOpY, getM subOpY)
                         where subOpX=parseExpr x (n+3) (m+1)
                               subOpY=parseExpr y (getN subOpX) (getM subOpX)
                               in1=newPortId n 
                               in2=newPortId (n+1)

parseExpr (x :<: y) n m=(Operator  (operatorId m) "<" [in1,in2] (Normal (newPortId (n+2))) () ,
                        [Wire (Just "bool") (outOf (myfst subOpX)) in1 ()
                        ,Wire (Just "bool") (outOf (myfst subOpY)) in2 ()] ++ (mysnd subOpX) ++ (mysnd subOpY),
                        [myfst subOpX,myfst subOpY] ++ (trd subOpX) ++ (trd subOpY), getN subOpY, getM subOpY)
                         where subOpX=parseExpr x (n+3) (m+1)
                               subOpY=parseExpr y (getN subOpX) (getM subOpX)
                               in1=newPortId n 
                               in2=newPortId (n+1)

parseExpr (x :<=: y) n m=(Operator  (operatorId m) "<=" [in1,in2] (Normal (newPortId (n+2))) () ,
                        [Wire (Just "bool") (outOf (myfst subOpX)) in1 ()
                        ,Wire (Just "bool") (outOf (myfst subOpY)) in2 ()] ++ (mysnd subOpX) ++ (mysnd subOpY),
                        [myfst subOpX,myfst subOpY] ++ (trd subOpX) ++ (trd subOpY), getN subOpY, getM subOpY)
                         where subOpX=parseExpr x (n+3) (m+1)
                               subOpY=parseExpr y (getN subOpX) (getM subOpX)
                               in1=newPortId n 
                               in2=newPortId (n+1)

parseExpr (x :>: y) n m=(Operator  (operatorId m) ">" [in1,in2] (Normal (newPortId (n+2))) () ,
                        [Wire (Just "bool") (outOf (myfst subOpX)) in1 ()
                        ,Wire (Just "bool") (outOf (myfst subOpY)) in2 ()] ++ (mysnd subOpX) ++ (mysnd subOpY),
                        [myfst subOpX,myfst subOpY] ++ (trd subOpX) ++ (trd subOpY), getN subOpY, getM subOpY)
                         where subOpX=parseExpr x (n+3) (m+1)
                               subOpY=parseExpr y (getN subOpX) (getM subOpX)
                               in1=newPortId n 
                               in2=newPortId (n+1)

parseExpr (x :>=: y) n m=(Operator  (operatorId m) ">=" [in1,in2] (Normal (newPortId (n+1))) () ,
                        [Wire (Just "bool") (outOf (myfst subOpX)) in1 ()
                        ,Wire (Just "bool") (outOf (myfst subOpY)) in2 ()] ++ (mysnd subOpX) ++ (mysnd subOpY),
                        [myfst subOpX,myfst subOpY] ++ (trd subOpX) ++ (trd subOpY), getN subOpY, getM subOpY)
                         where subOpX=parseExpr x (n+2) (m+1)
                               subOpY=parseExpr y (getN subOpX) (getM subOpX)
                               in1=newPortId n 
                               in2=newPortId (n+1)

parseExpr (Neg x) n m=(Operator  (operatorId m) "neg" [in1] (Normal (newPortId (n+1))) () ,
                        [Wire (Just "num") (outOf (myfst subOpX)) in1 ()] ++ (mysnd subOpX),
                        [myfst subOpX] ++ (trd subOpX), getN subOpX, getM subOpX)
                         where subOpX=parseExpr x (n+2) (m+1)
                               in1=newPortId n 

parseExpr (Pos x) n m=(Operator  (operatorId m) "pos" [in1] (Normal (newPortId (n+2))) () ,
                        [Wire (Just "num") (outOf (myfst subOpX)) in1 ()] ++ (mysnd subOpX),
                        [myfst subOpX] ++ (trd subOpX), getN subOpX, getM subOpX)
                         where subOpX=parseExpr x (n+3) (m+1)
                               in1=newPortId n 

 -- soortgelijk kan gedaan worden voor Adding Operators,Multiplying Operators en Shift Operators en Miscellaneous Operators

--parseExpr (PrimFCall (FCall (NSimple x), as)) n m=(Function (parseId x) (Just (parseId x)) inports outPort ([],[]) (), undefined,undefined,n+undefined,m+undefined)
--                                                  where inports=undefined
--                                                        outPort=undefined
--                                                        subParse=map parseAssocElem as

parseAssocElem x=undefined

{-voorbeeld primFcall:
(PrimFCall (FCall (NSimple "to_signed") 
				[Nothing :=>: ADExpr (PrimLit "1")
				,Nothing :=>: ADExpr (PrimLit "8")
				])) Nothing
-}



--hier zijn er nog geen inpoorten. Mogelijk kunnen die nog komen omdat (PrimFCall FCall) nog niet gedefinieerd is...
--bevendien snapte ik het nut van Choice niet echt...
{-
parseExpr (Aggregate eas) n m=Function (operatorId m) Nothing [] out (map trd subElems,map mysnd subElems) ()
                              where out=Tuple (newPortId n) (map outOf (myfst (subElems))
                                    subElems=map parseElemAssoc eas
                                    parseElemAssoc (ElemAssoc Others e)=[parseExpr e (n+1) (m+1)]
                                    parseElemAssoc (ElemAssoc (ChoiceE e1) e2)=parseExpr (Dummy e1 e2) (n+1) (m+1)

parseExpr (Dummy x y) n m=(Operator  (operatorId m) "??" [in1,in2] (Normal (newPortId (n+2))) () ,
                        [Wire (Just "bool") (outOf (myfst subOpX)) in1 ()
                        ,Wire (Just "bool") (outOf (myfst subOpY)) in2 ()] ++ (mysnd subOpX) ++ (mysnd subOpY),
                        [myfst subOpX,myfst subOpY] ++ (trd subOpX) ++ (trd subOpY), getN subOpY, getM subOpY)
                         where subOpX=parseExpr x (n+3) (m+1)
                               subOpY=parseExpr y (getN subOpX) (getM subOpX)
                               in1=newPortId n 
                               in2=newPortId (n+1)


-}

--parseExpr ::Expr-> num-> num-> (ArchElem,[Wire],[ArchElem],num,num)
{-
-}


 

outOf::ArchElem a-> PortId
outOf (Operator q w es r t)=extract (r)
                            where extract (Normal x)=x
outOf (Literal q v o a)=extract (o)
                        where extract (Normal x)=x

getN (o,ws,ops,n,m)=n
getM (o,ws,ops,n,m)=m

parseId::  VHDLId-> Id
parseId s=fromVHDLId s

--later deze verder implementeren
--parseVHDLName (NSimple s)=ParseSimpleName s
--parseVHDLName  (NSelected s)=parseSelectedName s
--parseVHDLName  (NIndexed s)=parseIndexedName s
--parseVHDLName  (NSlice s)=parseSliceName s
--parseVHDLName  (NAttribute s)=parseAttibName s

{-





{-
Eerst dacht ik dat een VHDL naam rechtstreeks naar een ID vertaalde, maar dat doet hij volgens mij niet altijd...
//??:
ParseSimpleName ::SimpleName-> Id
ParseSimpleName  s=ParseId s

//??:
parseSelectedName::SelectedName-> Id
parseSelectedName (x :.: y)=(parsePrefix x) ++ (parseSuffix y)

parsePrefix x=parseVHDLName x

parseSuffix:: Suffixx-> Id
parseSuffix (SSimple s)=ParseSimpleName s
parseSuffix (All)=""

parseIndexedName (IndexedName x es)=undefined
parseSliceName s=undefined
parseAttibName s=undefined
-}

--over de inwendige componenten is hier niets te zeggen, dus worden deze als lege lijsten terug gegeven.
parseEntityDec::EntityDec-> num-> Function
parseEntityDec (EntityDec id isds) n=Function (parseId id) (Just (parseId id)) ins (turnIdsToOut outs n) ([],[]) ()
                                     where isdsParsed=map parseEntityDec isds
                                           ins=getIns isdsParsed
                                           outs=getOuts isdsParsed
data Out = Normal PortId | Tuple PortId [Out]     deriving (Show)

parseMode (In)=True
parseMode (Out)=False

getIns::[(Id,bool)]-> [Id]
getIns=filter (x-> mysnd x) 

getOuts::[(Id,bool)]-> [Id]
getOuts=filter (x-> not(mysnd x)) 

turnIdsToOut::[Id]-> num-> Out
turnIdsToOut [x] n =Normal x
turnIdsToOut [] n  =Normal "deze wordt nooit gebruikt; niet tekenen."
turnIdsToOut x n =Tuple (newPortId (n)) (map turnIdToOud x)
                  where turnIdToOud y=Normal y

parseEntityDec (IfaceSigDec id m type)=((parseId id),parseMode m)  ||met het type van het signaal wordt nog niets gedaan









{-
voorbeeldAST:

LUEntity geeft de volgende info:
plus1Component_0 heeft vier signalen waarvan 1 uit en 3 in. vooral "xzcoY3" en "reszcp6zcp63"zijn hier van belang. 
Er wordt aangegeven welk signaal een ingang is en welke een uitgang. plus1Component_0 zou kunnen worden gezien als een functie met 3 ingangen ("reszcp6zcp63","clock" en "resetn") en ��n uitgang ("reszcp6zcp63"). Dus:
Function plus1Component_0 (Just plus1Component_0) ["reszcp6zcp63","clock","resetn"] "reszcp6zcp63" ([??],[??]) () zou uit deze date gehaald kunnen worden.
([??],[??]) hier omdat de inwendige componenten en draden hier niet in te vinden zijn.

In LUArch geeft de VHDL naam (NSimple "plus1Component_0")  aan dat hier "plus1Component_0" verder gespecificeerd wordt.
Welke informatie het VHDLId "structural" hier geeft is verder onbekend.

In de lijst met BlockDecItem's kunnen subprogram bodies en signal declarations staan. 
In dit geval alleen signaal declaraties. 
Deze signaal declaraties hebben geen expressies. Ze zijn van type signed_8,maar dat is nu even niet relevant.
De inwendige signalen "uzcp23" en "argzcpczcpc4" worden hier aangemaakt.

De tweede lijst is de lijst met concurrent statements. 
Dit voorbeeld bevat alleen concurrent_signal_assignment_statement type statements.
Deze hebben geen when-else en geen When, maar alleen een Wform met daarin een lijst van Wform elementen. 
Deze hebben de vorm WformElem expr (mayby expr). 
De eerste heeft expressie PrimName (NSimple "uzcp23") en verwijst naar het signaal "uzcp23". 
Dit eerste deel geeft aan dat de uitgang "reszcp6zcp63" gekoppeld is aan "uzcp23".

De tweede concurrent statement telt de uitkomsten van signalen "xzcoY3" en "argzcpczcpc4" op en zet deze in "uzcp23".              

De derde doet een function call met de naam "to_signed".De associatie elementen hebben alleen een eigenlijk deel (voor de pijl staat Nothing).
De eerste waarde in de lijst met associatieelementen is een expressie: de literal 1 en de tweede soortgelijk de literal 8.
Op de een of andere manier wordt dit dan gekoppeld aan "argzcpczcpc4, maar ik zie niet in hoe.......

("plus1Component_0",DesignFile 
	[	Use (NSelected (NSimple "work" :.: SSimple "types") :.: All), ++ "." ++ 
		Use (NSimple "work" :.: All),Library "IEEE",
		Use (NSelected (NSimple "IEEE" :.: SSimple "std_logic_1164") :.: All),
		Use (NSelected (NSimple "IEEE" :.: SSimple "numeric_std") :.: All),
		Use (NSelected (NSimple "std" :.: SSimple "textio") :.: All)
	] 
	[	LUEntity (EntityDec "plus1Component_0" 
		[	IfaceSigDec "xzcoY3" In "signed_8",
			IfaceSigDec "reszcp6zcp63" Out "signed_8",
			IfaceSigDec "clock" In "std_logic",
			IfaceSigDec "resetn" In "std_logic"
		])
		,LUArch (ArchBody "structural" (NSimple "plus1Component_0") 
		[	BDISD (SigDec "uzcp23" "signed_8" Nothing),
			BDISD (SigDec "argzcpczcpc4" "signed_8" Nothing)
		] 
		[	CSSASm (NSimple "reszcp6zcp63" :<==: ConWforms [] 
				(Wform [WformElem (PrimName (NSimple "uzcp23")) Nothing]) 
				Nothing)
			,CSSASm (NSimple "uzcp23" :<==: ConWforms [] (Wform [WformElem (PrimName (NSimple "xzcoY3") :+: PrimName (NSimple "argzcpczcpc4")) Nothing]) Nothing)
			,CSSASm (NSimple "argzcpczcpc4" :<==: ConWforms [] (Wform 
			[WformElem (PrimFCall (FCall (NSimple "to_signed") 
				[Nothing :=>: ADExpr (PrimLit "1")
				,Nothing :=>: ADExpr (PrimLit "8")
				])) Nothing
			])
			Nothing)
		])
	])

-}



-}


-- eerste op zet voor de datastructuur die we op gaan leveren richting de GUI

data ArchElem a =
    Function2 Id (Maybe Name) [InPort] OutPort ([ArchElem a],[Wire a]) a|
    Operator Id Name [In] Out a |
    Literal Id Value Out a |
    Mux Id [In] Out In a |
    Register Id (Maybe In) Out a
    deriving (Show)

data Wire a = Wire (Maybe Name) PortId PortId a     deriving (Show)
--                     naam     source dest

type Name = String

type PortId = Id
type In = PortId
data Out = Normal PortId | Tuple PortId [Out]     deriving (Show)


type Port = Out
type InPort = Port
type OutPort = Port

type Value = String
type Id = String

{-
-- dat is en het macc voorbeeld uit een van de clash papers
macc :: ArchElem ()
macc = Function "macc" (Just "macc")
        [(Normal "inX"), (Normal "inY")] -- inputs
        (Normal "outS'")                               -- output
         ([   -- implementatie
          Operator "*_0" "*" ["inX","inY"] (Normal "*_res") (),
          Operator "+_0" "+" ["plusInA", "plusInB"] (Normal "s'") (),
          Register "Reg0" (Just "regIn") (Normal "s") ()
         ],
         [Wire Nothing "?" "inX" (),
          Wire Nothing "?" "inY" (),
          Wire Nothing "s'" "outS" (),
          Wire Nothing "*_res" "plusInB" (),
          Wire Nothing "s" "plusInA" (),
          Wire Nothing "s'" "regIn" (),
          Wire Nothing "s'" "outS" ()
           ])
           ()


-}

