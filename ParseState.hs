module ParseState (parseState) where
import Language.VHDL.AST
import Datastruct
import ParseVHDL

parseState :: BlockSm -> [(String,Port)] -> EnvSession (ArchElem (), Backtrack)
parseState (BlockSm l _ _ _ [state_reset,state_update]) portTable = parseStateupdate state_update portTable

parseStateupdate :: ConcSm -> [(String,Port)] -> EnvSession (ArchElem (), Backtrack)
parseStateupdate (CSPSm (ProcSm statelabel [clockId,resetId,resvalid] [statement])) portTable
  = do
    -- TODO: make this smarter, able to handle variations in the structure
    -- right now itessentially the reverse of CLasH.VHDL.Generate.mkStateProcSm
      n1 <- getNewId
      n2 <- getNewId
      n3 <- getNewId
      let
        IfSm resetn_is_low [res_assign] clk_statement Nothing = statement
        [ElseIf rising_edge_clk [clk_assign]] = clk_statement
        SigAssign ({-varToVHDLName-} old) wform = clk_assign
        Wform [WformElem {-(PrimName {-$ varToVHDLName-} new)-}newExpr Nothing] = wform
        inportId = "regIn" ++ show n2 --newPortId
        inport = SinglePort inportId   -- TODO: gebruik hier de types, ga niet zelf een poort maken
        outport = SinglePort $ "regOut" ++ show n3 -- newPortId -- TODO: gebruik hier de types, ga niet zelf een poort maken
        reg = Register ((show old)++(show n1)) (Just $ inport) outport ()
        ref = PortReference (SinglePort $ parseVHDLName old)
      subexpr <- parseExpr "WTF-is-s?" portTable newExpr
      let
        reswires = (Wire Nothing (outOf (archElem subexpr)) inportId ()) : wires subexpr
        otherElems = archElem subexpr :  prevArchElems subexpr
        NSimple oldName = old
      return (ref,Backtrack{archElem=reg,wires=reswires,prevArchElems=otherElems})
