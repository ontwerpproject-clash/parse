

parseExpr ::Expr-> Int -> Int -> (ArchElem (),[Wire ()],[ArchElem ()],Int,Int)



-- Vincent code
parseExpr (PrimFCall x) n m = parseFCall x n m 

-- Maarten code
--parseExpr (PrimFCall (FCall (NSimple x), as)) n m=(Function (parseId x) (Just (parseId x)) inports outPort ([],[]) (), undefined,undefined,n+undefined,m+undefined)
--                                                  where inports=undefined
--                                                        outPort=undefined
--                                                        subParse=map parseAssocElem as

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