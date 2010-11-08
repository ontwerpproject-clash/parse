{-
===========================================================
=== Parser from VHDL.AST to ArchElem 					===
===========================================================

Goal: to translate the VHDL.AST to the ArchElem format that
will be use in the GUI to exhibit the architecture of the 
hardware speficief in Haskell.

-}
import Language.VHDL.AST
import Data.Char (toLower)
import Text.Regex.Posix
import Error



operatorId::num -> String
operatorId m= "A string"

newPortId m= "A string"

myfst (q,w,e,r,t)=q
mysnd (q,w,e,r,t)=w
trd (q,w,e,r,t)=e

outOf::ArchElem a-> PortId
outOf (Operator q w es r t)=extract (r)
                            where extract (Normal x)=x
outOf (Literal q v o a)=extract (o)
                        where extract (Normal x)=x
outOf (PortReference p)=extract (p)
                        where extract (SinglePort x)=x
                              extract (MultiPort x [y])= extract y  --dit werkt voor de huidige manier van selected names parsen
                        


getN (o,ws,ops,n,m)=n
getM (o,ws,ops,n,m)=m


--=== Vincent edit ===--
--====================--

parseArchBody 

--TODO: CSBSm BlockSm	& CSISm CompInsSm	 & CSPSm ProcSm	 & CSGSm GenerateSm 
parseConcSm(CSSASm x) = parseConSigAssignSm x

parseConSigAssignSm(x :<==: y) n m = (a, w, as, b, c)
						where
							(a, w, as, b, c) = parseConWforms y n m
							-- nog een soort van tweede iteratie die de x linkt aan de input van een andere operator

--parseConWforms( whenelse x maybewhen ) n m = 

-- Recursief om alle Wform's te behandelen maar de tussenresultaten mee te geven aan de volgende, voor unieke portId's en om alles Elementen en Wires te verzamelen
parseWform(x:xs) = ("",w:[res_w],aw:[res_aw],res_n,res_m)
	where
		(res_a,res_w,res_aw,n,m) = parseWform xs
		(a,w,aw,res_n,res_m) = parseWformElem x n m
parseWform(x:[]) = parseWformElem x n m

parseWformElem x = parseExpr x 	--Maybe Expr <- moet nog worden geimplementeerd

-- Lelijk om fst te gebruiken, en wellicht ook niet goed. Moeten we eerst weten hoe FCall zijn opgebouwd en wat je met de primlit's moet doen.
parseFCall (FCall name x) n m = (Literal (literalId l) (fst x) n (), [],	[]		, n, m)
 

 
--=== Marten code ==---


parseAssocElem x=undefined

parseId::  VHDLId-> Id
parseId s=fromVHDLId s
--later deze verder implementeren
--parseVHDLName (NSimple s)=ParseSimpleName s
--parseVHDLName  (NSelected s)=parseSelectedName s
--parseVHDLName  (NIndexed s)=parseIndexedName s
--parseVHDLName  (NSlice s)=parseSliceName s
--parseVHDLName  (NAttribute s)=parseAttibName s

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


{-
--over de inwendige componenten is hier niets te zeggen, dus worden deze als lege lijsten terug gegeven.
parseEntityDec::EntityDec-> num-> Function
parseEntityDec (EntityDec id isds) n=Function (parseId id) (Just (parseId id)) ins (turnIdsToOut outs n) ([],[]) ()
                                     where isdsParsed=map parseEntityDec isds
                                           ins=getIns isdsParsed
                                           outs=getOuts isdsParsed

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

-}

