package sygus

sealed trait Cmd
case class SortDefCmd(sym: String, sortExpr: SortExpr) extends Cmd
case class VarDeclCmd(sym: String, sortExpr: SortExpr) extends Cmd
case class PrimedVarDeclCmd(sym: String, sortExpr: SortExpr) extends Cmd
case class FunDeclCmd(sym: String, sortExprs: List[SortExpr], sortExpr: SortExpr) extends Cmd
case class FunDefCmd(sym: String, list: List[(String, SortExpr)], se: SortExpr, t: Term) extends Cmd
case class SynthInvCmd(sym: String, list: List[(String, SortExpr)]) extends Cmd
case class SynthFunCmd14(override val sym: String,
                         override val list: List[(String, SortExpr)],
                         override val se: SortExpr,
                         ntDefs: List[NTDef])
    extends SynthFunCmd(sym, list, se)

// SynthFunCmd ::= SynthFunCmd14 | (synth-fun Symbol ((Symbol SortExpr)∗ ) SortExpr)
case class SynthFunCmd16(override val sym: String,
                         override val list: List[(String, SortExpr)],
                         override val se: SortExpr)
    extends SynthFunCmd(sym, list, se)

class SynthFunCmd(val sym: String,
                  val list: List[(String, SortExpr)],
                  val se: SortExpr) extends Cmd

case class ConstraintCmd(t: Term) extends Cmd
case class PreconditionCmd(t: Term) extends Cmd
case class InvConstraintCmd(inv: String, pre: String, trans: String, post: String) extends Cmd
case class CheckSynthCmd() extends Cmd
case class SetOptsCmd(list: List[(String, String)]) extends Cmd

// End ///////////////////////////////////////////////////////////////
