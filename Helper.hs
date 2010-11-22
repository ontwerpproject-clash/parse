module Helper where

import Datastruct

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

-- Get the outport of an Architecture element
outportOf :: ArchElem a -> Port
outportOf (Function _ _ _ p _ _) = p
outportOf (Operator _ _ _ p _) = p
outportOf (Literal _ _ p _) = p
outportOf (Mux _ _ p _ _) = p
outportOf (Register _ _ p _) = p
outportOf (PortReference p) = p