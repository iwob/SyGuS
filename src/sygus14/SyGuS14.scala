package sygus14

import scala.util.parsing.combinator._

/**
 * @see http://www.sygus.org/SyGuS-COMP2014.html 
 */

case class SyGuS14(setLogic: Option[SyGuS14.SetLogicCmd],cmds: List[SyGuS14.Cmd])

///////////////////////////////////

object SyGuS14 {

  import jeep.lang.Diag
  
  def apply(expr: String): Either[String,SyGuS14] = 
    new Parser().parse(expr)   
  
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

  /////////////////////////////////
  
  sealed trait Literal
  case class IntConst(value: Int) extends Literal
  case class RealConst(value: Double) extends Literal
  case class BoolConst(value: Boolean) extends Literal
  case class BVConst(value: List[Boolean]) extends Literal 
  case class EnumConst(sort: String, ctor: String) extends Literal
  
  /////////////////////////////////

  sealed trait Term
  case class CompositeTerm(symbol: String,terms: List[Term]) extends Term
  case class LiteralTerm(literal: Literal) extends Term
  case class SymbolTerm(symbol: String) extends Term  
  case class LetTerm(list: List[(String,SortExpr,Term)],term: Term) extends Term

  sealed trait GTerm
  case class CompositeGTerm(symbol: String,terms: List[GTerm]) extends GTerm
  case class LiteralGTerm(literal: Literal) extends GTerm
  case class SymbolGTerm(symbol: String) extends GTerm  
  case class LetGTerm(list: List[(String,SortExpr,GTerm)],term: GTerm) extends GTerm
  // Since we're not checking semantics at this point, GenericGTerm
  // is used as a placeholder for the other GTerms of Section 3.6, i.e. 
  // Constant, Variable, InputVariable LocalVariable 
  case class GenericGTerm(identifier: String,sortExpr: SortExpr) extends GTerm
  
  final case class NTDef(symbol: String,sortExpr:SortExpr,gterms: List[GTerm])
  
  /////////////////////////////////

  case class SetLogicCmd(id: SetLogicTheory)
  
  sealed trait Cmd;
  case class SortDefCmd(sym: String, sortExpr: SortExpr) extends Cmd 
  case class VarDeclCmd(sym: String, sortExpr: SortExpr) extends Cmd
  case class FunDeclCmd(sym: String, sortExprs: List[SortExpr], sortExpr: SortExpr) extends Cmd
  case class FunDefCmd(sym: String,list: List[(String,SortExpr)],se: SortExpr,t: Term) extends Cmd
  case class SynthFunCmd(sym: String,list: List[(String,SortExpr)],se: SortExpr,ntDefs: List[NTDef]) extends Cmd
  case class ConstraintCmd(t: Term) extends Cmd
  case class CheckSynthCmd() extends Cmd
  case class SetOptsCmd(list: List[(String,String)]) extends Cmd
  
  /////////////////////////////////
  
  case class SyGuSParserException(msg: String) extends RuntimeException(msg)
  
  class Parser extends JavaTokenParsers {

    val reservedWords = Set( "set-logic", "define-sort", "declare-var", 
      "declare-fun", "define-fun", "synth-fun", "constraint", "check-synth", "set-options",
      "BitVec", "Array", "Int", "Bool", "Enum", "Real", "Constant", "Variable", "InputVariable", 
      "LocalVariable", "let", "true", "false" )  
    
    // FIXME - hack because escaping '-' in the regex doesn't seem to work       
    def symbol = "-" | 
      "[a-zA-Z_\\−\\+\\*&\\|\\!~<>=/%\\?\\.\\$\\^]([a-zA-Z0-9_\\+\\−\\*&\\|\\!~<>=/%\\?\\.\\$\\^])*".r ^^ {
      case s => if( reservedWords.contains( s ) ) throw new SyGuSParserException(s"symbol expected, found reserved word: $s") else s      
    }

    val boolean: Parser[Boolean] = "true" ^^^ { true } | "false" ^^^ { false }
    val genericGtermToken: Parser[String] = "Constant" | "Variable" | "InputVariable" | "LocalVariable" | symbol    

    /////////////////////////////////

    def sortExpr = "Int" ^^^ { IntSortExpr() } |
      "Bool" ^^^ { BoolSortExpr() } |
      "Real" ^^^ { RealSortExpr() } |
      "(" ~ "BitVec" ~ wholeNumber ~ ")" ^^ { 
        case _ ~ _ ~ i ~ _ => BitVecSortExpr(i.toInt) 
      } 
      "(" ~> "Enum" ~> rep1(symbol) <~ ")" ^^ { 
        case list => EnumSortExpr(list)
      } |
      "(" ~> "Array" ~> sortExpr ~ sortExpr <~ ")" ^^ { 
        case se1 ~ se2 => ArraySortExpr(se1,se2)
      } |
      symbol ^^ { sym => SymbolSortExpr(sym) }    

    /////////////////////////////////

    def intConst: Parser[IntConst] = wholeNumber ^^ { x => IntConst(x.toInt) }
    def realConst: Parser[RealConst] = floatingPointNumber ^^ { x => RealConst(x.toDouble) }
    def boolConst: Parser[BoolConst] = boolean ^^ { b => BoolConst(b) }
    def bvConst: Parser[BVConst] =  {
      def bitsToBV(str: String): List[Boolean] = str.toList.map { x => if( x == '0' ) false else true }
      def hexToBV(str: String): List[Boolean] = bitsToBV(new java.math.BigInteger(str, 16).toString(2))
      ( "#b" ~ rep1( "[01]".r ) ) ^^ { case _ ~ list => BVConst(bitsToBV(list.mkString)) } | 
      ( "#x" ~ rep1( "[a-fA-F0-9]".r ) ) ^^ { case _ ~ list => BVConst(hexToBV(list.mkString)) }
    }
  
    def enumConst: Parser[EnumConst] = symbol ~ "::" ~ symbol ^^ { case a ~ _ ~ b => EnumConst(a,b) }
  
    def literal: Parser[Literal] = intConst | realConst | boolConst | bvConst | enumConst 
  
    /////////////////////////////////
  
    def letTerm: Parser[LetTerm] = {
      def entry: Parser[(String,SortExpr,Term)] = "(" ~ symbol ~ sortExpr ~ term ~ ")" ^^ {
        case _ ~ a ~ b ~ c ~ _ => (a,b,c)         
      }
      "(let" ~ "(" ~ rep1(entry) ~ ")" ~ term ~ ")" ^^ {
        case _ ~ _ ~ list ~ _ ~ t ~ _ => LetTerm(list,t) 
      }
    }
    
    def compositeTerm: Parser[CompositeTerm] = "(" ~ symbol ~ rep(term) ~ ")" ^^ {
      case _ ~ sym ~ list ~ _ => CompositeTerm(sym,list)
    }
  
    def literalTerm: Parser[LiteralTerm] = literal ^^ { x => LiteralTerm(x) }
    def symbolTerm: Parser[SymbolTerm] = symbol ^^ { x => SymbolTerm(x) }
  
    def term: Parser[Term] = letTerm | compositeTerm | literalTerm | symbolTerm 
  
    /////////////////////////////////

    def compositeGTerm: Parser[CompositeGTerm] = "(" ~> symbol ~ rep(gterm) <~ ")" ^^ {
      case sym ~ list => CompositeGTerm(sym,list)
    }
    
    def literalGTerm: Parser[LiteralGTerm] = literal ^^ { x => LiteralGTerm(x) }
    def symbolGTerm: Parser[SymbolGTerm] = symbol ^^ { x => SymbolGTerm(x) }
    def letGTerm: Parser[LetGTerm] = {
      def entry: Parser[(String,SortExpr,GTerm)] = "(" ~ symbol ~ sortExpr ~ gterm ~ ")" ^^ {
        case _ ~ a ~ b ~ c ~ _ => (a,b,c)         
      }
      "(let" ~> "(" ~> rep1(entry) ~ ")" ~ gterm <~ ")" ^^ {
        case list ~ _ ~ t => LetGTerm(list,t) 
      }
    }
    
    def genericGTerm: Parser[GenericGTerm] = "(" ~> genericGtermToken ~ sortExpr <~ ")" ^^ {
      case tok ~ se => GenericGTerm(tok,se)
    }
  
    def gterm: Parser[GTerm] = letGTerm | compositeGTerm | literalGTerm | symbolGTerm | genericGTerm
    
    // QUOTEDLIT               "\""([a-z]|[A-Z]|{DIGIT}|".")+"\""
    val quotedLiteral: Parser[String] = "\"[a-zA-Z0-9\\.]([a-zA-Z0-9\\.])*\"".r

    // NTDef : TK_LPAREN Symbol SortExpr TK_LPAREN GTermPlus TK_RPAREN TK_RPAREN
    
    def ntDef: Parser[NTDef] = "(" ~> symbol  ~ 
      sortExpr ~ "(" ~ rep1(gterm) <~ ")" <~ ")" ^^ {
      case sym ~ se ~ _ ~ list => NTDef(sym, se, list )     
    }
  
    /////////////////////////////////
  
    def setLogicCmd: Parser[SetLogicCmd] = "(set-logic" ~> symbol <~ ")" ^^ {
      case sym => SetLogicCmd(Enum.valueOf(classOf[SetLogicTheory],sym)) 
    }
    
    def sortDefCmd: Parser[SortDefCmd] = "(define-sort" ~> symbol ~ sortExpr  <~ ")" ^^ { 
      case sym ~ se => SortDefCmd(sym,se) 
    }
  
    def varDeclCmd: Parser[VarDeclCmd] = "(declare-var" ~> symbol ~ sortExpr <~ ")" ^^ { 
      case sym ~ se => VarDeclCmd(sym,se) 
    } 
  
    def funDeclCmd: Parser[FunDeclCmd] = "(declare-fun" ~ symbol ~ "(" ~ rep(sortExpr) ~ ")" ~ sortExpr ~ ")" ^^ {
      case _ ~ sym ~ _ ~ list ~ _ ~ se ~ _ => FunDeclCmd(sym,list,se)
    }

    def funDefCmd: Parser[FunDefCmd] = {
      def entry: Parser[(String,SortExpr)] = "(" ~ symbol ~ sortExpr ~ ")" ^^ { case _ ~ s ~ e ~ _  => (s,e) }    
      "(define-fun" ~ symbol ~ "(" ~ rep( entry ) ~ ")" ~ sortExpr ~ term ~ ")" ^^ {
        case _ ~ sym ~ _ ~ list ~ _ ~ se ~ t ~ _ => FunDefCmd(sym,list,se,t)
      }
    }
  
    def synthFunCmd: Parser[SynthFunCmd] = { 
      def entry: Parser[(String,SortExpr)] = "(" ~ symbol ~ sortExpr ~ ")" ^^ { case _ ~ s ~ e ~ _ => (s,e) }    
      "(synth-fun" ~ symbol ~ "(" ~ rep(entry) ~ ")" ~ sortExpr ~ 
      "(" ~ rep1(ntDef) ~ ")" ~ ")" ^^ {
        case _ ~ sym ~ _ ~ list ~ _ ~ se ~ _ ~ list2 ~ _ ~ _ => SynthFunCmd(sym,list,se,list2)
      }
    }
  
    def constraintCmd: Parser[ConstraintCmd] = "(constraint" ~ term ~ ")" ^^ { 
      case _ ~ t ~ _ => ConstraintCmd(t) }
    def checkSynthCmd: Parser[CheckSynthCmd] = "(check-synth)" ^^^ { CheckSynthCmd() }
  
    def setOptsCmd: Parser[SetOptsCmd] = {
      def entry: Parser[(String,String)] = "(" ~ symbol ~ quotedLiteral ~ ")" ^^ { case _ ~ s ~ q ~ _ => (s,q) }  
      "(set-options" ~> "(" ~> rep1( entry ) <~ ")" <~ ")" ^^ {
        case list => SetOptsCmd( list )     
      }
    }
  
    def cmd: Parser[Cmd] = sortDefCmd |   varDeclCmd | funDeclCmd | funDefCmd |
      synthFunCmd | constraintCmd | checkSynthCmd | setOptsCmd /* | failure("not a cmd") */

    ///////////////////////////////////

    def syGuS14: Parser[SyGuS14] = opt(setLogicCmd) ~ rep1(cmd) ^^ { 
      case slc ~ cmds => SyGuS14( slc,cmds ) 
    }

    ///////////////////////////////////
    
    def validate[T](parser: Parser[T],expr: String): Either[String,T] = {
      parseAll(phrase(parser), expr) match {
      // parseAll(parser, expr) match {      
        case m @ Failure(msg, next) => Left(s"Parse failure: $msg" )
        case m @ Error(msg, next) => Left(s"Parse error: $msg" )
        case Success(result, next) => Right(result)
      }
    }
    
    def parse(expr: String): Either[String,SyGuS14] =
      validate(syGuS14, expr)
  }
}

// End ///////////////////////////////////////////////////////////////
