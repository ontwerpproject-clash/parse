module Whiteboxtest where

-- local imports
import Datastruct
import ParseVHDL
import ParseExpr

-- VHDL Imports
import Language.VHDL.AST hiding (Function)

-- HUnit Imports
import Test.HUnit

{-
TestCase has the following structure for assertEqual's:
  Assertion message   :: String
  Expected result     :: a
  Actual result       :: a

Expected result is the value that is expected when evaluating this testcase. The Actual 
result is the result of the testcase and is compared to the expected result.
-}
tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3]

test1 = TestCase (assertEqual "for parseVHDLName" "testport1" (ParseVHDL.parseVHDLName (NSimple (Basic "testport1"))) )
test2 = TestCase (assertEqual "for parseExpr :+:" ((Operator "" "+" ["newPortId0","newPortId1"] (SinglePort "newPortId2") ()),[],[],1,1) (parseExpr ((PrimName (NSimple (Basic "port1"))) :+: (PrimName (NSimple (Basic "port2"))) ) 1 1 ) )
test3 = TestCase (assertEqual "for FindInof" ("", (Wire Nothing "" "" ())) (findInof "bla" []) )