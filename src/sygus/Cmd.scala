package sygus

sealed trait Cmd;
case class SortDefCmd(sym: String, sortExpr: SortExpr) extends Cmd
case class VarDeclCmd(sym: String, sortExpr: SortExpr) extends Cmd
case class FunDeclCmd(sym: String, sortExprs: List[SortExpr], sortExpr: SortExpr) extends Cmd
case class FunDefCmd(sym: String, list: List[(String, SortExpr)], se: SortExpr, t: Term) extends Cmd
case class SynthFunCmd14(sym: String, list: List[(String, SortExpr)], se: SortExpr, ntDefs: List[NTDef]) extends Cmd

// SynthFunCmd ::= SynthFunCmd14 | (synth-fun Symbol ((Symbol SortExpr)∗ ) SortExpr)
case class SynthFunCmd16(sym: String, 
  list: List[(String, SortExpr)], 
  se: SortExpr) extends Cmd

case class ConstraintCmd(t: Term) extends Cmd
case class CheckSynthCmd() extends Cmd
case class SetOptsCmd(list: List[(String, String)]) extends Cmd

// End ///////////////////////////////////////////////////////////////
