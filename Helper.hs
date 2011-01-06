module Helper where

import Datastruct
import Data.Maybe (isNothing)

-- Get elements out of a three tuple
get1out3 (x,y,z)=x
get2out3 (x,y,z)=y
get3out3 (x,y,z)=z

-- Get elements out of a five tuple
get1out5 (q,_,_,_,_)=q
get2out5 (_,w,_,_,_)=w
get3out5 (_,_,e,_,_)=e
get4out5 (_,_,_,n,_)=n
get5out5 (_,_,_,_,m)=m

get1out6 (q,_,_,_,_,_)=q
get2out6 (_,w,_,_,_,_)=w
get3out6 (_,_,e,_,_,_)=e
get4out6 (_,_,_,n,_,_)=n
get5out6 (_,_,_,_,m,_)=m
get6out6 (_,_,_,_,_,x)=x


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

--verbind de meegegeven geparse delen aan het meegegeven architectuurElement.
connect:: [(ArchElem (),[Wire ()],[ArchElem ()],Int,Int)] -> ArchElem () -> String -> (ArchElem (),[Wire ()],[ArchElem ()],Int,Int)
connect xs m name
     =(m,allWires,allElems,finalN,finalM)
       where
        newWires=map makeNewWire (zip fromOuts toIns)
        fromOuts=map (getHighest.outportOf.get1out5) xs
        toIns=map getHighest (inportsOf m)
        allElems=(map get1out5 xs) ++ concat (map get3out5 xs)
        allWires=newWires ++ concat (map get2out5 xs)
        finalN= get4out5 (last xs)
        finalM= get5out5 (last xs)
        makeNewWire (x,i)=Wire (Just name) x i ()

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
