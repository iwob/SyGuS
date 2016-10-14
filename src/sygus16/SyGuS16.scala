package sygus16

import scala.util.parsing.combinator._

import sygus._

///////////////////////////////////

case class SyGuS16(setLogic: Option[sygus14.SyGuS14.SetLogicCmd], cmds: List[Cmd])

///////////////////////////////////

  
///////////////////////////////////

object SyGuS16 {

  import sygus14._
  
  class Parser extends sygus14.SyGuS14.Parser {
  
    private def synthFunCmd16: Parser[SynthFunCmd16] = {
      def entry: Parser[(String, SortExpr)] = "(" ~ symbol ~ sortExpr ~ ")" ^^ { case _ ~ s ~ e ~ _ => (s, e) }
      "(synth-fun" ~ symbol ~ "(" ~ rep(entry) ~ ")" ~ sortExpr ~ ")" ^^ {
          case _ ~ sym ~ _ ~ list ~ _ ~ se ~ _ => SynthFunCmd16(sym, list, se)
        }
    }
    
    def cmd16: Parser[Cmd] = sortDefCmd | varDeclCmd | funDeclCmd | funDefCmd |
      synthFunCmd14 | synthFunCmd16 | constraintCmd | checkSynthCmd | setOptsCmd

    ///////////////////////////////////

    def syGuS16: Parser[SyGuS16] = opt(setLogicCmd) ~ rep1(cmd16) ^^ {
      case slc ~ cmds => SyGuS16(slc, cmds)
    }
    ///////////////////////////////////

    override def validate[T](parser: Parser[T], expr: String): Either[String, T] = {
      parseAll(phrase(parser), expr) match {
        // parseAll(parser, expr) match {      
        case m @ Failure(msg, next) => Left(s"Parse failure: $msg")
        case m @ Error(msg, next)   => Left(s"Parse error: $msg")
        case Success(result, next)  => Right(result)
      }
    }

    def parseSyGuS16(expr: String): Either[String, SyGuS16] =
      validate(syGuS16, expr)
  }

  /////////////////////////////////
  
  def parseSyGuS16Text(str: String): Either[String, SyGuS16] = {
    val lines = str.split("\\r\\n|\\n|\\r").filter { x => !x.trim.startsWith(";") || x.trim.isEmpty }
    val text = lines.mkString("\n")
    // jeep.lang.Diag.println(text)
    val parser = new SyGuS16.Parser

    parser.parseSyGuS16(text)
  }

  def parseSyGuS16File(f: java.io.File): Either[String, SyGuS16] =
    parseSyGuS16Text(scala.io.Source.fromFile(f).mkString)
}

// End ///////////////////////////////////////////////////////////////
