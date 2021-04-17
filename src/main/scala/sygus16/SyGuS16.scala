package sygus16

import scala.util.parsing.combinator._

import sygus._

///////////////////////////////////

case class SyGuS16(setLogic: Option[sygus14.SyGuS14.SetLogicCmd], cmds: List[Cmd])
  
///////////////////////////////////

object SyGuS16 {

  import sygus14._
  
  class Parser(strict: Boolean = true) extends sygus14.SyGuS14.Parser(strict) {
  
    override def sortExpr = 
      "String" ^^^ { StringSortExpr() } |      
      "Int" ^^^ { IntSortExpr() } |
      "Bool" ^^^ { BoolSortExpr() } |
      "Real" ^^^ { RealSortExpr() } |
      "(" ~ "BitVec" ~ wholeNumber ~ ")" ^^ {
        case _ ~ _ ~ i ~ _ => BitVecSortExpr(i.toInt)
      }
    "(" ~> "Enum" ~> rep1(symbol) <~ ")" ^^ {
      case list => EnumSortExpr(list)
    } |
      "(" ~> "Array" ~> sortExpr ~ sortExpr <~ ")" ^^ {
        case se1 ~ se2 => ArraySortExpr(se1, se2)
      } |
      symbol ^^ { sym => SymbolSortExpr(sym) }
    
    private def synthFunCmd16: Parser[SynthFunCmd16] = {
      def entry: Parser[(String, SortExpr)] = "(" ~ symbol ~ sortExpr ~ ")" ^^ { case _ ~ s ~ e ~ _ => (s, e) }
      "(" ~> "synth-fun" ~> symbol ~ "(" ~ rep(entry) ~ ")" ~ sortExpr ~ ")" ^^ {
          case sym ~ _ ~ list ~ _ ~ se ~ _ => SynthFunCmd16(sym, list, se)
        }
    }
    
    def stringConst: Parser[Literal] = stringLiteral ^^ { s => StringConst(s.substring(1, s.size-1)) } // "\"" ~ symbol ~ "\"" ^^ { case _ ~ xx ~ _ => StringConst(xx) }
    override def literal: Parser[Literal] = stringConst | super.literal
    
    def cmd16: Parser[Cmd] = sortDefCmd | varDeclCmd | funDeclCmd | funDefCmd |
      synthFunCmd16 | synthFunCmd14 | constraintCmd | preconditionCmd | checkSynthCmd | setOptsCmd|
      invConstraintCmd | primedVarDeclCmd | synthInvCmd
      // ^^ { case x => jeep.lang.Diag.println( x ); x }

    ///////////////////////////////////

    def syGuS16: Parser[SyGuS16] = opt(setLogicCmd) ~ rep1(cmd16) ^^ {
      case slc ~ cmds => SyGuS16(slc, cmds)
    }
    
    ///////////////////////////////////

    override def validate[T](parser: Parser[T], expr: String): Either[String, T] = {
      val text = removeComments(expr)
      parseAll(phrase(parser), text) match {
        // parseAll(parser, expr) match {      
        case m @ Failure(msg, next) => Left(s"Parse failure: $msg, ${next.source}")
        case m @ Error(msg, next)   => Left(s"Parse error: $msg")
        case Success(result, next)  => Right(result)
      }
    }

    def parseSyGuS16(expr: String): Either[String, SyGuS16] =
      validate(syGuS16, expr)
  }

  /////////////////////////////////
  
  def parseSyGuS16Text(str: String, strict: Boolean = true): Either[String, SyGuS16] = {
    // jeep.lang.Diag.println(text)
    val parser = new SyGuS16.Parser(strict)

    parser.parseSyGuS16(str)
  }

  def parseSyGuS16File(f: java.io.File, strict: Boolean = true): Either[String, SyGuS16] =
    parseSyGuS16Text(scala.io.Source.fromFile(f).mkString, strict)
}

// End ///////////////////////////////////////////////////////////////
