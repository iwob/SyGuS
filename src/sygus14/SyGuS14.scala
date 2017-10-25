package sygus14

import sygus._

import scala.util.parsing.combinator._
import java.io.File

/**
  * @see http://www.sygus.org/SyGuS-COMP2014.html
  */

case class SyGuS14(setLogic: Option[SyGuS14.SetLogicCmd], cmds: List[Cmd])

///////////////////////////////////

object SyGuS14 {

  import jeep.lang.Diag

  def apply(expr: String): Either[String, SyGuS14] =
    new Parser().parse(expr)

  /////////////////////////////////

  case class SetLogicCmd(id: SetLogicTheory)

  /////////////////////////////////

  case class SyGuSParserException(msg: String) extends RuntimeException(msg)

  class Parser extends JavaTokenParsers {

    val reservedWords = Set("set-logic", "define-sort", "declare-var",
      "declare-fun", "define-fun", "synth-fun", "constraint", "check-synth", "set-options",
      "BitVec", "Array", "Int", "Bool", "Enum", "Real", "Constant", "Variable", "InputVariable",
      "LocalVariable", "let", "true", "false" 
      // , "forall", "exists"
      )

    val unguardedSymbolRegex = """[a-zA-Z\-[_\+\*&\|\!~<>=/%\?\.\$\^]]([a-zA-Z0-9\-[_\+\*&\|\!~<>=/%\?\.\$\^]])*""".r
    val guardedSymbolRegex = """[a-zA-Z\-[_\+\*&\!'~<>=/%\?\.\$\^]]([a-zA-Z0-9\-[_\+\*&\!'~<>=/%\?\.\$\^]])*""".r    

    val guardedSymbol: Parser[String] = "|" ~ guardedSymbolRegex ~ "|" ^^ { case _ ~ s ~ _ => s"|$s|" }
    
    val symbol = (guardedSymbol | unguardedSymbolRegex ) ^^ {
      case s => if (reservedWords.contains(s)) 
        throw new SyGuSParserException(s"symbol expected, found reserved word: $s") 
      else s
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
        case se1 ~ se2 => ArraySortExpr(se1, se2)
      } |
      symbol ^^ { sym => SymbolSortExpr(sym) }

    /////////////////////////////////

    def intConst: Parser[IntConst] = wholeNumber ^^ { x => IntConst(x.toInt) }
    def realConst: Parser[RealConst] = floatingPointNumber ^^ { x => RealConst(x.toDouble) }
    def boolConst: Parser[BoolConst] = boolean ^^ { b => BoolConst(b) }
    def bvConst: Parser[BVConst] = {
      def bitsToBV(str: String): List[Boolean] = str.toList.map { x => if (x == '0') false else true }
      def hexToBV(str: String): List[Boolean] = bitsToBV(new java.math.BigInteger(str, 16).toString(2))
      ("#b" ~ rep1("[01]".r)) ^^ { case _ ~ list => BVConst(bitsToBV(list.mkString)) } |
        ("#x" ~ rep1("[a-fA-F0-9]".r)) ^^ { case _ ~ list => BVConst(hexToBV(list.mkString)) }
    }

    def enumConst: Parser[EnumConst] = symbol ~ "::" ~ symbol ^^ { case a ~ _ ~ b => EnumConst(a, b) }

    def literal: Parser[Literal] = intConst | realConst | boolConst | bvConst | enumConst

    /////////////////////////////////

    def letTerm: Parser[LetTerm] = {
      def entry: Parser[(String, SortExpr, Term)] = "(" ~ symbol ~ sortExpr ~ term ~ ")" ^^ {
        case _ ~ a ~ b ~ c ~ _ => (a, b, c)
      }
      "(" ~> "let" ~ "(" ~ rep1(entry) ~ ")" ~ term ~ ")" ^^ {
        case _ ~ list ~ _ ~ t ~ _ => LetTerm(list, t)
      }
    }

    def compositeTerm: Parser[CompositeTerm] = "(" ~ symbol ~ rep(term) ~ ")" ^^ {
      case _ ~ sym ~ list ~ _ => CompositeTerm(sym, list)
    }

    def literalTerm: Parser[LiteralTerm] = literal ^^ { x => LiteralTerm(x) }
    def symbolTerm: Parser[SymbolTerm] = symbol ^^ { x => SymbolTerm(x) }
    
    def forallTerm: Parser[ForallTerm] = "(" ~> "forall" ~> "(" ~> 
      rep(symbolAndSortExpr) ~ ")" ~ term <~ ")" ^^ { case sorts ~ _ ~ term  => 
      ForallTerm(sorts,term)
    }
      
    def existsTerm: Parser[ExistsTerm] = "(" ~> "exists" ~> "(" ~> 
      rep(symbolAndSortExpr) ~ ")" ~ term <~ ")" ^^ { case sorts ~ _ ~ term  => 
      ExistsTerm(sorts,term)
    }

    def term: Parser[Term] = forallTerm | existsTerm | letTerm | compositeTerm | literalTerm | symbolTerm

    /////////////////////////////////

    def compositeGTerm: Parser[CompositeGTerm] = "(" ~> symbol ~ rep(gterm) <~ ")" ^^ {
      case sym ~ list => CompositeGTerm(sym, list)
    }

    def literalGTerm: Parser[LiteralGTerm] = literal ^^ { x => LiteralGTerm(x) }
    def symbolGTerm: Parser[SymbolGTerm] = symbol ^^ { x => SymbolGTerm(x) }
    def letGTerm: Parser[LetGTerm] = {
      def entry: Parser[(String, SortExpr, GTerm)] = "(" ~ symbol ~ sortExpr ~ gterm ~ ")" ^^ {
        case _ ~ a ~ b ~ c ~ _ => (a, b, c)
      }
      "(" ~> "let" ~> "(" ~> rep1(entry) ~ ")" ~ gterm <~ ")" ^^ {
        case list ~ _ ~ t => LetGTerm(list, t)
      }
    }

    def genericGTerm: Parser[GenericGTerm] = "(" ~> genericGtermToken ~ sortExpr <~ ")" ^^ {
      case tok ~ se => GenericGTerm(tok, se)
    }

    def gterm: Parser[GTerm] = letGTerm | compositeGTerm | literalGTerm | symbolGTerm | genericGTerm

    // QUOTEDLIT               "\""([a-z]|[A-Z]|{DIGIT}|".")+"\""
    val quotedLiteral: Parser[String] = "\"[a-zA-Z0-9\\.]([a-zA-Z0-9\\.])*\"".r

    // NTDef : TK_LPAREN Symbol SortExpr TK_LPAREN GTermPlus TK_RPAREN TK_RPAREN

    def ntDef: Parser[NTDef] = "(" ~> symbol ~
      sortExpr ~ "(" ~ rep1(gterm) <~ ")" <~ ")" ^^ {
        case sym ~ se ~ _ ~ list => NTDef(sym, se, list)
      }

    /////////////////////////////////

    def setLogicCmd: Parser[SetLogicCmd] = "(" ~> "set-logic" ~> symbol <~ ")" ^^ {
      case sym => SetLogicCmd(Enum.valueOf(classOf[SetLogicTheory], sym))
    }

    def sortDefCmd: Parser[SortDefCmd] = "(" ~> "define-sort" ~> symbol ~ sortExpr <~ ")" ^^ {
      case sym ~ se => SortDefCmd(sym, se)
    }

    def varDeclCmd: Parser[VarDeclCmd] = "(" ~> "declare-var" ~> symbol ~ sortExpr <~ ")" ^^ {
      case sym ~ se => VarDeclCmd(sym, se)
    }

    def funDeclCmd: Parser[FunDeclCmd] = "(" ~> "declare-fun" ~ symbol ~ "(" ~ rep(sortExpr) ~ ")" ~ sortExpr ~ ")" ^^ {
      case _ ~ sym ~ _ ~ list ~ _ ~ se ~ _ => FunDeclCmd(sym, list, se)
    }

    def funDefCmd: Parser[FunDefCmd] = {
      def entry: Parser[(String, SortExpr)] = "(" ~ symbol ~ sortExpr ~ ")" ^^ { case _ ~ s ~ e ~ _ => (s, e) }
      "(" ~> "define-fun" ~> symbol ~ "(" ~ rep(entry) ~ ")" ~ sortExpr ~ term ~ ")" ^^ {
        case sym ~ _ ~ list ~ _ ~ se ~ t ~ _ => FunDefCmd(sym, list, se, t)
      }
    }

    def symbolAndSortExpr: Parser[(String, SortExpr)] = "(" ~ symbol ~ sortExpr ~ ")" ^^ { case _ ~ s ~ e ~ _ => (s, e) }
      
    def synthFunCmd14: Parser[SynthFunCmd14] = {
      "(" ~> "synth-fun" ~> symbol ~ "(" ~ rep(symbolAndSortExpr) ~ ")" ~ sortExpr ~
        "(" ~ rep1(ntDef) ~ ")" ~ ")" ^^ {
          case sym ~ _ ~ list ~ _ ~ se ~ _ ~ list2 ~ _ ~ _ => SynthFunCmd14(sym, list, se, list2)
        }
    }

    def constraintCmd: Parser[ConstraintCmd] = "(" ~> "constraint" ~> term <~ ")" ^^ {
      ConstraintCmd(_)
    }

    def checkSynthCmd: Parser[CheckSynthCmd] = "(" ~> "check-synth" <~ ")" ^^^ { CheckSynthCmd() }

    def setOptsCmd: Parser[SetOptsCmd] = {
      def entry: Parser[(String, String)] = "(" ~ symbol ~ quotedLiteral ~ ")" ^^ { case _ ~ s ~ q ~ _ => (s, q) }
      "(" ~> "set-options" ~> "(" ~> rep1(entry) <~ ")" <~ ")" ^^ {
        case list => SetOptsCmd(list)
      }
    }

    def cmd14: Parser[Cmd] = sortDefCmd | varDeclCmd | funDeclCmd | funDefCmd |
      synthFunCmd14 | constraintCmd | checkSynthCmd | setOptsCmd /* | failure("not a cmd") */

    ///////////////////////////////////

    def syGuS14: Parser[SyGuS14] = opt(setLogicCmd) ~ rep1(cmd14) ^^ {
      case slc ~ cmds => SyGuS14(slc, cmds)
    }

    ///////////////////////////////////

    def validate[T](parser: Parser[T], expr: String): Either[String, T] = {
      parseAll(phrase(parser), expr) match {
        // parseAll(parser, expr) match {      
        case m @ Failure(msg, next) => Left(s"Parse failure: $msg, ${next.source}")
        case m @ Error(msg, next)   => Left(s"Parse error: $msg, ${next.source}")
        case Success(result, next)  => Right(result)
      }
    }

    def parse(expr: String): Either[String, SyGuS14] =
      validate(syGuS14, expr)
  }

  def parseSyGuS14Text(str: String): Either[String, SyGuS14] = {
    val lines = str.split("\\r\\n|\\n|\\r").filter { x => !x.trim.startsWith(";") || x.trim.isEmpty }
    val text = lines.mkString("\n")
    // jeep.lang.Diag.println(text)
    val parser = new SyGuS14.Parser

    parser.parse(text)
  }

  def parseSyGuS14File(f: File): Either[String, SyGuS14] =
    parseSyGuS14Text(scala.io.Source.fromFile(f).mkString)

}

// End ///////////////////////////////////////////////////////////////
