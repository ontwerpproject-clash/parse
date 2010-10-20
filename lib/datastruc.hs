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


------------------------------------------
-- onderstaande code is niet op voorbeelden getest, er is dus geen gerantie dat het helemaal goed werkt..

--In de bijeenkomst hadden we het over een sturcttur met meerdere uitgangspoorten i.p.v. onze genenste strucruur.
--Dit zorg ervoor dat voor tuples alle pijlen afzonderlijk moeten worden bijgehouden.
-- Hieronder staat een parser die functies in onze oude structuur omzet naar deze alternatieve structuur
-- Omzetting van ArchElems anders dan functies gaat op soorgelijke manier.
--deze alternatieve structuur heeft dus geen echt voordeel boven de huidige structuur aanhouden bij het parsen
-- aangezien deze structuur uit de huidige structuur gehaald kan worden.

--Vanuit deze nieuwe structuur kan ook verder geparsed worden zodat de Wires tussen elementen zitten
-- i.p.v. tussen poorten. Dit komt dan neer op voor elke draad zoeken naar in welk
-- element de ingangspoort en in welk element de uitgangspoort van die draad zich bevind.
--Dit zoeken moet verder gebeuren in acht houdend alle subcomponenten van de huidige functie (door nesting van de wires
-- hoeft niet dieper worden gezocht voor het parsen van 1 functie)
type OutPort2 = Id

data ArchElem3 a =
    Function3 Id (Maybe Name) [InPort] [OutPort2] ([ArchElem3 a],[Wire a]) a
    deriving (Show)

outOf::ArchElem a-> PortId
outOf (Operator q w es r t)=extract (r)
                            where extract (Normal x)=x
outOf (Literal q v o a)=extract (o)
                        where extract (Normal x)=x

getN (o,ws,ops,n,m)=n
getM (o,ws,ops,n,m)=m


parseOut:: Out-> ( [PortId],[(PortId,[PortId]) ] )
parseOut (Normal x) = ([x],[])
parseOut (Tuple x ys)=(outs,reprs)
                       where outs =fst res
                             reprs= [(x, fst res)]++ (snd res)
                             res  =unzipM (map parseOut ys)

unzipM:: [([a],[b])]-> ([a],[b])
unzipM xs = (concat (map fst xs),concat (map snd xs))

parse::ArchElem a-> ArchElem3 a
parse (Function2 x y ins out (as,ws) a)
   = Function3 x y ins (fst outParsed) ((map parse as), splitwires ws (snd outParsed)) a
     where outParsed=parseOut out



splitwires ((Wire x f t a):ws) reprs |reprOfF==[]=(Wire x f t a): (splitwires ws reprs)
                                     |otherwise  = map (\y->(Wire x y t a)) (snd (unzipM reprs)) ++ (splitwires ws reprs)
                                                  where reprOfF =filter (\y-> fst y==f) reprs


