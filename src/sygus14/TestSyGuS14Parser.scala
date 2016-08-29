package sygus14

import java.io._

import org.junit._
import org.junit.Assert._

class TestSyGuS14Parser {

  import SyGuS14._
  
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
  def testCmds: Unit = {
    
    val parser = new Parser    

    assertEquals( Right(SetLogicCmd("LIA")), parser.validate(parser.setLogicCmd, "(set-logic LIA)")  )    
    assertEquals( Right(CheckSynthCmd()), parser.validate(parser.cmd, "(check-synth)")  )
    
    assertEquals( Right(VarDeclCmd("x",IntSortExpr())), parser.validate(parser.varDeclCmd, "(declare-var x Int)")  )    
    assertEquals( Right(VarDeclCmd("x",IntSortExpr())), parser.validate(parser.cmd, "(declare-var x Int)")  )    
    
    jeep.lang.Diag.println(parser.validate(parser.cmd, "(constraint (>= (max2 x y) x))" ))
    assertTrue( parser.validate(parser.cmd, "(constraint (>= (max2 x y) x))" ).isRight )
    

        
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
  
    def synthFunCmd: Parser[SynthFunCmd] = { 
      def entry: Parser[(String,SortExpr)] = symbol ~ sortExpr ^^ { case s ~ e => (s,e) }    
      "(synth-fun" ~ symbol ~ "(" ~ rep(entry) ~ ")" ~ sortExpr ~ rep1(ntDef) ~ ")" ^^ {
        case _ ~ sym ~ _ ~ list ~ _ ~ se ~ list2 ~ _ => SynthFunCmd(sym,list,se,list2)
      }
    }
  
    def checkSynthCmd: Parser[CheckSynthCmd] = "(check-synth)" ^^^ { CheckSynthCmd() }
  
    def setOptsCmd: Parser[SetOptsCmd] = {
****/
  }
  
  /////////////////////////////////

  @Test
  def testExample: Unit = {
     val path = System.getProperty("user.dir") + "/resources/example.txt"
     assertTrue( parseSyGuS14File(new java.io.File(path)).isRight )
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
