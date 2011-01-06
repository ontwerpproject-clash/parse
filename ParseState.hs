module ParseState (parseState) where
import Language.VHDL.AST
import Datastruct
import ParseExpr
import ParseVHDL
import Helper

parseState :: BlockSm -> [(String,Port)] -> Int -> Int -> (ArchElem (),(ArchElem (),[Wire ()],[ArchElem ()],Int,Int))
parseState (BlockSm l _ _ _ [state_reset,state_update]) portTable n m = parseStateupdate state_update portTable n m

parseStateupdate :: ConcSm -> [(String,Port)] -> Int -> Int -> (ArchElem (),(ArchElem (),[Wire ()],[ArchElem ()],Int,Int))
parseStateupdate (CSPSm (ProcSm statelabel [clockId,resetId,resvalid] [statement])) portTable n m
  = (ref,(reg,wires,otherElems,get4out5 subexpr,get5out5 subexpr))
  where
    -- TODO: make this smarter, able to handle variations in the structure
    -- right now itessentially the reverse of CLasH.VHDL.Generate.mkStateProcSm
    IfSm resetn_is_low [res_assign] clk_statement Nothing = statement
    [ElseIf rising_edge_clk [clk_assign]] = clk_statement
    SigAssign ({-varToVHDLName-} old) wform = clk_assign
    Wform [WformElem {-(PrimName {-$ varToVHDLName-} new)-}newExpr Nothing] = wform
    inportId = "regIn" ++ show m --newPortId m
    inport = SinglePort inportId   -- TODO: gebruik hier de types, ga niet zelf een poort maken
    outport = SinglePort $ "regOut" ++ show (m+1) -- newPortId $ m+1 -- TODO: gebruik hier de types, ga niet zelf een poort maken
    reg = Register ((show old)++(show n)) (Just $ inport) outport ()
    ref = PortReference (SinglePort $ parseVHDLName old)
    subexpr=parseExpr "WTF-is-s?" portTable newExpr (n+1) (m+2)
    wires = (Wire Nothing (outOf (get1out5 subexpr)) inportId ()) : get2out5 subexpr
    otherElems = get1out5 subexpr :  get3out5 subexpr
    NSimple oldName = old
