package sygus14

import scala.util.parsing.combinator._

/**
 * @see http://www.sygus.org/SyGuS-COMP2014.html 
 */

class SyGuS14Parser extends JavaTokenParsers {

  // FIXME - which of these are regex characters that should be escaped?
  val specialChar = "[_+−*&|!~<>=/%?.$^]".r 
  
  def symbol: Parser[String] = ( "[a-zA-Z]".r | specialChar ) ~ rep( "[a-zA-Z0-9]".r | specialChar ) ^^ { 
    case hd ~ tl => hd ++ tl.mkString }

  def setLogicCmd: Parser[SetLogicCmd] = "(set-logic" ~ symbol ~ ")" ^^ { 
    case _ ~ sym ~ _ => SetLogicCmd(sym) 
  }
  
  /////////////////////////////////
  
  sealed trait SortExpr
  case class IntSortExpr() extends SortExpr 
  case class BoolSortExpr() extends SortExpr
  case class RealSortExpr() extends SortExpr
  case class BitVecSortExpr(n: Int) extends SortExpr {
    require( n > 0 )    
  }
  case class EnumSortExpr(symbols: List[String]) extends SortExpr
  case class ArraySortExpr(sortExpr1: SortExpr,sortExpr2: SortExpr) extends SortExpr
  case class SymbolSortExpr(symbol: String) extends SortExpr

  def sortExpr: Parser[SortExpr] = "Int" ^^^ { IntSortExpr() } |
    "Bool" ^^^ { BoolSortExpr() } |
    "Real" ^^^ { RealSortExpr() } |
    "(" ~ "BitVec" ~ wholeNumber ~ ")" ^^ { 
      case _ ~ _ ~ i ~ _ => BitVecSortExpr(i.toInt) 
    } 
    "(" ~ "Enum" ~ rep1(symbol) ~ ")" ^^ { 
      case _ ~ _ ~ list ~ _ => EnumSortExpr(list)
    } |
    "(" ~ "Array" ~ sortExpr ~ sortExpr ~ ")" ^^ { 
      case _ ~ _ ~ se1 ~ se2 ~ _ => ArraySortExpr(se1,se2)
    } |
    symbol ^^ { sym => SymbolSortExpr(sym) }    

  /////////////////////////////////
    
  sealed trait Literal
  case class IntConst(value: Int) extends Literal
  case class RealConst(value: Double) extends Literal
  case class BoolConst(value: Boolean) extends Literal
  case class BVConst(value: List[Boolean]) extends Literal 
  case class EnumConst(sort: String, ctor: String) extends Literal

  def intConst: Parser[IntConst] = wholeNumber ^^ { x => IntConst(x.toInt) }
  def realConst: Parser[RealConst] = floatingPointNumber ^^ { x => RealConst(x.toDouble) }
  def boolConst: Parser[BoolConst] = "true" ^^^ { BoolConst(true) } | "false" ^^^ { BoolConst(false) }
  def bvConst: Parser[BVConst] =  {
    def bitsToBV(str: String): List[Boolean] = str.toList.map { x => if( x == '0' ) false else true }
    def hexToBV(str: String): List[Boolean] = bitsToBV(new java.math.BigInteger(str, 16).toString(2))
    ( "#b" ~ rep1( "[01]".r ) ) ^^ { case _ ~ list => BVConst(bitsToBV(list.mkString)) } | 
    ( "#x" ~ rep1( "[a-fA-F0-9]".r ) ) ^^ { case _ ~ list => BVConst(hexToBV(list.mkString)) }
  }
  def enumConst: Parser[EnumConst] = symbol ~ "::" ~ symbol ^^ { case a ~ _ ~ b => EnumConst(a,b) }
  
  def literal: Parser[Literal] = ( intConst | realConst | boolConst | bvConst | enumConst ) ^^ {
    x => x    
  }
  
  /////////////////////////////////
  
  sealed trait Term
  case class CompositeTerm(symbol: String,terms: List[Term]) extends Term
  case class LiteralTerm(literal: Literal) extends Term
  case class SymbolTerm(symbol: String) extends Term  
  case class LetTerm(list: List[(String,SortExpr,Term)],term: Term) extends Term

  def letTerm: Parser[LetTerm] = {
    def entry: Parser[(String,SortExpr,Term)] = ???   
    "(let" ~ "(" ~ rep1(entry) ~ ")" ~ term ~ ")" ^^ {
      case _ ~ _ ~ list ~ _ ~ t ~ _ => LetTerm(list,t) 
    }
  }
    
  def compositeTerm: Parser[CompositeTerm] = "(" ~ symbol ~ rep(term) ~ ")" ^^ {
    case _ ~ sym ~ list ~ _ => CompositeTerm(sym,list)
  }
  def literalTerm: Parser[LiteralTerm] = literal ^^ { x => LiteralTerm(x) }
  def symbolTerm: Parser[SymbolTerm] = symbol ^^ { x => SymbolTerm(x) }
  
  def term: Parser[Term] = ( compositeTerm | literalTerm | symbolTerm | letTerm ) ^^ {  
    case t => t
  }
  
  /////////////////////////////////
  
  sealed trait GTerm  
  final case class NTDef(symbol: String,sortExpr:SortExpr,gterms: List[GTerm])

  def gterm: Parser[GTerm] = ???  
  val quotedLiteral: Parser[String] = "([a-zA-Z0-9.])+".r
  def ntDef: Parser[NTDef] = "(" ~ symbol ~ sortExpr ~ rep(gterm) ~ ")" ^^ {
    case _ ~ sym ~ se ~ list ~ _ => NTDef(sym, se, list )     
  }
  
  /////////////////////////////////

  case class SetLogicCmd(id: String)
  
  sealed trait Cmd;
  case class SortDefCmd(sym: String, sortExpr: SortExpr) extends Cmd 
  case class VarDeclCmd(sym: String, sortExpr: SortExpr) extends Cmd
  case class FunDeclCmd(sym: String, sortExprs: List[SortExpr], sortExpr: SortExpr) extends Cmd
  case class FunDefCmd(sym: String,list: List[(String,SortExpr)],se: SortExpr,t: Term) extends Cmd
  case class SynthFunCmd(sym: String,list: List[(String,SortExpr)],se: SortExpr,ntDefs: List[NTDef]) extends Cmd
  case class ConstraintCmd(t: Term) extends Cmd
  case class CheckSynthCmd() extends Cmd
  case class SetOptsCmd(list: List[(String,String)]) extends Cmd
  
  def sortDefCmd: Parser[SortDefCmd] = "(define-sort" ~ symbol ~ sortExpr  ~ ")" ^^ { 
    case _ ~ sym ~ se ~ _ => SortDefCmd(sym,se) }
  
  def varDeclCmd: Parser[VarDeclCmd] = "(declare-var" ~ symbol ~ sortExpr ~ ")" ^^ { 
    case _ ~ sym ~ se ~ _ => VarDeclCmd(sym,se) } 
  
  def funDeclCmd: Parser[FunDeclCmd] = "(declare-fun" ~ symbol ~ "(" ~ rep(sortExpr) ~ ")" ~ sortExpr ~ ")" ^^ {
    case _ ~ sym ~ _ ~ list ~ _ ~ se ~ _ => FunDeclCmd(sym,list,se)
  }

  def funDefCmd: Parser[FunDefCmd] = {
    def entry: Parser[(String,SortExpr)] = symbol ~ sortExpr ^^ { case s ~ e => (s,e) }    
    "(define-fun" ~ symbol ~ "(" ~ rep( entry ) ~ ")" ~ sortExpr ~ term ~ ")" ^^ {
      case _ ~ sym ~ _ ~ list ~ _ ~ se ~ t ~ _ => FunDefCmd(sym,list,se,t)
    }
  }
  
  def synthFunCmd: Parser[SynthFunCmd] = { 
    def entry: Parser[(String,SortExpr)] = symbol ~ sortExpr ^^ { case s ~ e => (s,e) }    
    "(synth-fun" ~ symbol ~ "(" ~ rep(entry) ~ ")" ~ sortExpr ~ rep1(ntDef) ~ ")" ^^ {
      case _ ~ sym ~ _ ~ list ~ _ ~ se ~ list2 ~ _ => SynthFunCmd(sym,list,se,list2)
    }
  }
  
  def constraintCmd: Parser[ConstraintCmd] = "(constraint" ~ term ~ ")" ^^ { 
    case _ ~ t ~ _ => ConstraintCmd(t) }
  def checkSynthCmd: Parser[CheckSynthCmd] = "(check-synth)" ^^^ { CheckSynthCmd() }
  
  def setOptsCmd: Parser[SetOptsCmd] = {
    def entry: Parser[(String,String)] = symbol ~ quotedLiteral ^^ { case s ~ q => (s,q) }  
    "(set-options" ~ "(" ~ rep1( entry ) ~ ")" ^^ {
      case _ ~ _ ~ list ~ _ => SetOptsCmd( list )     
    }
  }
  
  def cmd: Parser[Cmd] = sortDefCmd |   varDeclCmd | funDeclCmd | funDefCmd |
    synthFunCmd | constraintCmd | checkSynthCmd | setOptsCmd

  ///////////////////////////////////

  case class SyGuS14(setLogic: Option[SetLogicCmd],cmds: List[Cmd])  

  def syGuS14: Parser[SyGuS14] = opt(setLogicCmd) ~ rep1(cmd) ^^ { 
    case slc ~ cmds => SyGuS14( slc,cmds ) 
  }
    
  def parse(expr: String): Option[SyGuS14] = {
    parseAll(syGuS14, expr) match {
      case Failure(msg, next) => println(s"Could not parse - $msg" ); None
      case Error(msg, next) => println(s"Could not parse - $msg"); None
      case Success(result, next) => Some(result)
    }
  }
}

// End ///////////////////////////////////////////////////////////////
