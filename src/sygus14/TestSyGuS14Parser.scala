package sygus14

import java.io._

import org.junit._
import org.junit.Assert._

class TestSyGuS14Parser {

  import SyGuS14._
  
  /////////////////////////////////

  import scala.util.parsing.combinator._
  
  class ProcessedSourceParser extends JavaTokenParsers {
    // def processedSource: Parser[String] = "{" ~ "[\\s\\S]*".r ~  "()" ~ "}" ^^ { case _ ~ src ~ _ ~ _ => src }
    // def processedSource: Parser[String] = "{" ~ "[\\s\\S]*".r ~ "()\n}" ^^ { case a ~ b ~ c => b }
    // def processedSource: Parser[String] = "{" ~ """.*\(\)\n}]*""".r ^^ { case a ~ b => b }
    // def processedSource: Parser[String] = "{" ~ """.*?\(\)\n}""".r ^^ { case a ~ b => b }
    // def processedSource: Parser[String] = """\{.*(?<!\(\)\n\})(\(\)\n\})""".r
    // def processedSource: Parser[String] = """\{\n.*(?<!\(\)\n\})(\(\)\n\})""".r
    def processedSource: Parser[String] = "\\{((?:(?!\\(\\)\\s*\\})[\\s\\S])*)\\(\\)\\s*\\}".r
  }

  object ProcessedSourceParser {
    def parse(src: String): Either[String,String] = {
      val parser = new ProcessedSourceParser
      parser.parseAll(parser.processedSource,src) match {
        case m @ parser.Failure(msg, next) => Left(s"Parse failure: $msg" )
        case m @ parser.Error(msg, next) => Left(s"Parse error: $msg" )
        case parser.Success(result, next) => Right(result)
      }
    }
  }
  
  @Test
  def testProcessSource: Unit = {
    val src = """{
      sdajc sa ksmv fm
      ()
    }  
    """
    jeep.lang.Diag.println( ProcessedSourceParser.parse( src ) )    
  }
  
  /////////////////////////////////
    
  private def getRecursiveListOfFiles(dir: File): List[File] = {
    val these = dir.listFiles.toList
    these.filter(!_.isDirectory) ++ 
      these.filter(_.isDirectory).flatMap(getRecursiveListOfFiles)
  }
  
  /////////////////////////////////
  
  private def parseSyGuS14Text(str: String): Either[String,SyGuS14] = {
    val lines = str.split("\\r\\n|\\n|\\r").filter { x => !x.trim.startsWith(";") || x.trim.isEmpty }
    val text = lines.mkString("\n")
    // jeep.lang.Diag.println(text)
    val parser = new SyGuS14.Parser
    
    parser.parse(text)
  }

  private def parseSyGuS14File(f: File): Either[String,SyGuS14] =
    parseSyGuS14Text(scala.io.Source.fromFile(f).mkString)
  
  /////////////////////////////////

  @Test
  def testSymbol: Unit = {
    val parser = new Parser    
    assertEquals( Right("<="), parser.validate(parser.symbol, "<=")  )    
    assertEquals( Right("a0"), parser.validate(parser.symbol, "a0")  ) 
    assertEquals( Right("x"), parser.validate(parser.symbol, "x")  )     
  }

  @Test
  def testGTerm: Unit = {
    val parser = new Parser
    // FIXME: can't parse - for some reason, subsituting "minus"    
    val gterm = """
      (x y 0 1
        (ite StartBool Start Start)
        (minus Ftart Start)          
        (+ Atart Start)
      )"""
    
    jeep.lang.Diag.println(parser.validate(parser.gterm, gterm ) )
    assertTrue( parser.validate(parser.gterm, gterm ).isRight )    
  }
  
  /////////////////////////////////

  @Test
  def testNTDef: Unit = {
    val parser = new Parser

    // FIXME: can't parse - for some reason, subsituting "minus"
    val ntDef1 = """
    (Start Int 
        (x y 0 1
          (+ Start Start)
          (minus Start Start)
          (ite StartBool Start Start)
        )
    )"""
             
    // jeep.lang.Diag.println(parser.validate(parser.ntDef, ntDef1 ) )
    assertTrue( parser.validate(parser.ntDef, ntDef1 ).isRight )
    
    val ntDef2 = """    
    (StartBool Bool 
      (
        (and StartBool StartBool)
        (or StartBool StartBool)
        (not StartBool)
        (le Start Start)
        (eq Start Start)
        (ge Start Start)
      )
    )"""
jeep.lang.Diag.println(parser.validate(parser.ntDef, ntDef1 ) )
    assertTrue( parser.validate(parser.ntDef, ntDef2 ).isRight )
  }
  
  /////////////////////////////////  
  
  @Test
  def testSynthFunCmd: Unit = {
    val parser = new Parser    
    
    // FIXME: can't parse <=,=,>= for some reason, subsituting leq etc    
    val synthFun1 = """
    (synth-fun max2 ((x Int) (y Int)) Int
      (
        (Start Int 
          (x y 0 1
            (+ Start Start)
            (minus Start Start)
            (ite StartBool Start Start)
          )
        )
      )
    )"""

    assertTrue( parser.validate(parser.synthFunCmd, synthFun1 ).isRight )
// /**************    
    val synthFun2 = """
    (synth-fun max2 ((x Int) (y Int)) Int
      (
        (Start Int 
          (x y 0 1
            (+ Start Start)
            (minus Start Start)
            (ite StartBool Start Start)
          )
        )
        (StartBool Bool 
          (
            (and StartBool StartBool)
            (or StartBool StartBool)
            (not StartBool)
            (le Start Start)
            (eq Start Start)
            (ge Start Start)
          )
        )
      )
    )"""
    
    jeep.lang.Diag.println(parser.parse(parser.synthFunCmd, synthFun2 ))
    assertTrue( parser.validate(parser.synthFunCmd, synthFun2 ).isRight )
 //**************/  
  }
  
  @Test
  def testCmds: Unit = {
    
    val parser = new Parser    

    assertEquals( Right(SetLogicCmd("LIA")), parser.validate(parser.setLogicCmd, "(set-logic LIA)")  )    
    assertEquals( Right(CheckSynthCmd()), parser.validate(parser.cmd, "(check-synth)")  )
    
    assertEquals( Right(VarDeclCmd("x",IntSortExpr())), parser.validate(parser.varDeclCmd, "(declare-var x Int)")  )    
    assertEquals( Right(VarDeclCmd("x",IntSortExpr())), parser.validate(parser.cmd, "(declare-var x Int)")  )    
    
    // jeep.lang.Diag.println(parser.validate(parser.cmd, "(constraint (>= (max2 x y) x))" ))
    assertTrue( parser.validate(parser.cmd, "(constraint (>= (max2 x y) x))" ).isRight )
    assertTrue( parser.validate(parser.cmd, "(constraint (or (= x (max2 x y)) (= y (max2 x y))))" ).isRight )
    jeep.lang.Diag.println(parser.validate(parser.funDefCmd, "(define-fun iff ((a Bool) (b Bool)) Bool (not (xor a b)))" ) )    
    jeep.lang.Diag.println(parser.validate(parser.cmd, "(define-fun iff ((a Bool) (b Bool)) Bool (not (xor a b)))" ) )
    assertTrue( parser.validate(parser.cmd, "(define-fun iff ((a Bool) (b Bool)) Bool (not (xor a b)))" ).isRight )

    
    
        
/****    
    def sortDefCmd: Parser[SortDefCmd] = "(define-sort" ~ symbol ~ sortExpr  ~ ")" ^^ { 
      case _ ~ sym ~ se ~ _ => SortDefCmd(sym,se) }
  
    def funDeclCmd: Parser[FunDeclCmd] = "(declare-fun" ~ symbol ~ "(" ~ rep(sortExpr) ~ ")" ~ sortExpr ~ ")" ^^ {
      case _ ~ sym ~ _ ~ list ~ _ ~ se ~ _ => FunDeclCmd(sym,list,se)
    }

    def funDefCmd: Parser[FunDefCmd] = {
      def entry: Parser[(String,SortExpr)] = symbol ~ sortExpr ^^ { case s ~ e => (s,e) }    
      "(define-fun" ~ symbol ~ "(" ~ rep( entry ) ~ ")" ~ sortExpr ~ term ~ ")" ^^ {
        case _ ~ sym ~ _ ~ list ~ _ ~ se ~ t ~ _ => FunDefCmd(sym,list,se,t)
      }
    }
  
  
    def setOptsCmd: Parser[SetOptsCmd] = {
****/
  }
  
  /////////////////////////////////

  @Test
  def testExample: Unit = {
     val path = System.getProperty("user.dir") + "/resources/example.txt"
     val pr = parseSyGuS14File(new java.io.File(path))
     jeep.lang.Diag.println(pr)
     assertTrue( pr.isRight )
  }

  /////////////////////////////////
  
  @Test
  def testParser: Unit = {
    val root = System.getProperty("user.dir") + "/resources/sygus14"
    val files = getRecursiveListOfFiles(new File( root ) )

    var numParsed = 0
    for( f <- files ) {
      try {
        val result = parseSyGuS14File(f) 
        if( result.isRight ) {
          numParsed += 1
        }
        else {
          jeep.lang.Diag.println(result)          
        }
      }
      catch {
        case ex: Throwable => {
          jeep.lang.Diag.println(f)
          jeep.lang.Diag.println( ex.getMessage )
        }
      }
    }
    jeep.lang.Diag.println(s"files: ${files.length}, succesfully parsed: $numParsed")
    assertEquals( files.length, numParsed )
  }
}

// End ///////////////////////////////////////////////////////////////
