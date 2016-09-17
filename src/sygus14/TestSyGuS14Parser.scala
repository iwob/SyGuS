package sygus14

import java.io._

import org.junit._
import org.junit.Assert._

class TestSyGuS14Parser {

  import SyGuS14._
  // import scala.util.parsing.combinator._
  
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
    // FIXME: can't parse - for some reason, substituting "minus"    
    val gterm1 = """
      (x y 0 1
        (ite StartBool Start Start)
        (- Start Start)          
        (+ Start Start)
      )"""
    
    assertEquals( 
      Right( 
        CompositeGTerm( "x",
          List(SymbolGTerm("y"), LiteralGTerm(IntConst(0)), LiteralGTerm(IntConst(1)), 
            CompositeGTerm("ite",List(SymbolGTerm("StartBool"), SymbolGTerm("Start"), SymbolGTerm("Start"))), 
            CompositeGTerm("-",List(SymbolGTerm("Start"), SymbolGTerm("Start"))), 
            CompositeGTerm("+",List(SymbolGTerm("Start"), SymbolGTerm("Start"))))
        )
      ), parser.validate(parser.gterm, gterm1 ) )    
  }
  
  /////////////////////////////////

  @Test
  def testNTDef: Unit = {
    val parser = new Parser

    // FIXME: can't parse - for some reason, substituting "minus"
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
// jeep.lang.Diag.println(parser.validate(parser.ntDef, ntDef1 ) )
    assertTrue( parser.validate(parser.ntDef, ntDef2 ).isRight )
  }
  
  /////////////////////////////////  
  
  @Test
  def testSynthFunCmd: Unit = {
    val parser = new Parser    

    val synthFunSyGuSCOMP2014HtmlExample5 = """
    (synth-fun f (( x Int ) ( y Int )) Int
      (
        ( Start Int 
          ( x y z
            (+ Start Start )
            (let ( ( z Int Start ) ) Start)
          )
        )
      )
    )"""
    
    assertTrue( parser.validate(parser.synthFunCmd, synthFunSyGuSCOMP2014HtmlExample5 ).isRight )    
    
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
    
    assertTrue( parser.validate(parser.synthFunCmd, synthFun2 ).isRight )
  }
  
  @Test
  def testCmds: Unit = {
    
    val parser = new Parser    

    assertEquals( Right(SetLogicCmd(SetLogicTheory.LIA)), parser.validate(parser.setLogicCmd, "(set-logic LIA)")  )    
    assertEquals( Right(CheckSynthCmd()), parser.validate(parser.cmd, "(check-synth)")  )
    
    assertEquals( Right(VarDeclCmd("x",IntSortExpr())), parser.validate(parser.varDeclCmd, "(declare-var x Int)")  )    
    assertEquals( Right(VarDeclCmd("x",IntSortExpr())), parser.validate(parser.cmd, "(declare-var x Int)")  )    
    
    assertTrue( parser.validate(parser.cmd, "(constraint (>= (max2 x y) x))" ).isRight )
    assertTrue( parser.validate(parser.cmd, "(constraint (or (= x (max2 x y)) (= y (max2 x y))))" ).isRight )
    assertTrue( parser.validate(parser.cmd, "(define-fun iff ((a Bool) (b Bool)) Bool (not (xor a b)))" ).isRight )

    val setOpts = """(set-options ((samples "0")))"""
    assertTrue( parser.validate(parser.setOptsCmd, setOpts ).isRight )    
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
          jeep.lang.Diag.println(f)          
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
