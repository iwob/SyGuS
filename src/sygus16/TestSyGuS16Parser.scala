package sygus16

import java.io._

import org.junit._
import org.junit.Assert._

import sygus._

class TestSyGuS16Parser {

  import SyGuS16._
  
  /////////////////////////////////
    
  private def getRecursiveListOfFiles(dir: File): List[File] = {
    val these = dir.listFiles.toList
    these.filter(!_.isDirectory) ++ 
      these.filter(_.isDirectory).flatMap(getRecursiveListOfFiles)
  }
  
  /////////////////////////////////
  
  @Test
  def testExample: Unit = {
    val example = """
      (set-logic LIA)
      (synth-fun addExpr1 ((x Int) (y Int)) Int)
      (synth-fun addExpr2 ((x Int) (y Int)) Int)
      (declare-var x Int)
      (declare-var y Int)
      (constraint (= (+ (addExpr1 x y) (addExpr2 y x)) (- x y)))
      (check-synth)"""

     
    val pr = SyGuS16.parseSyGuS16Text(example)
    assertTrue( pr.isRight )
  }

  @Test
  def testExample2: Unit = {
    val example = """
(set-logic LIA)

(synth-fun eq1 ( (x Int) (y Int) (z Int) ) Int)

(define-fun iteB (( b1 Bool ) (b2 Bool ) (b3 Bool )) Bool ( or ( and b1 b2 ) ( and (not b1 ) b3 ) ) )
(define-fun plus2 ((b1 Int) (b2 Int)) Int ( + b1 b2))
(define-fun plus3 ((b1 Int) (b2 Int) (b3 Int)) Int ( +  ( + b1 b2) b3))
(define-fun plus4 ((b1 Int) (b2 Int) (b3 Int) (b4 Int)) Int ( +  ( plus3  b1 b2 b3) b4))
(define-fun plus5 ((b1 Int) (b2 Int) (b3 Int) (b4 Int) (b5 Int)) Int (+  ( plus4 b1 b2 b3 b4) b5))
(define-fun plus6 ((b1 Int) (b2 Int) (b3 Int) (b4 Int) (b5 Int) (b6 Int) ) Int (+  ( plus5 b1 b2 b3 b4  b5) b6  ))
(define-fun plus7 ((b1 Int) (b2 Int) (b3 Int) (b4 Int) (b5 Int) (b6 Int) (b7 Int)) Int (+  ( plus6 b1 b2 b3 b4  b5 b6 ) b7  ))
(define-fun plus8 ((b1 Int) (b2 Int) (b3 Int) (b4 Int) (b5 Int) (b6 Int) (b7 Int) (be Int)) Int (+  ( plus7 b1 b2 b3 b4  b5 b6 b7) be  ))
(define-fun plus9 ((b1 Int) (b2 Int) (b3 Int) (b4 Int) (b5 Int) (b6 Int) (b7 Int) (be Int) (bn Int)) Int (+  ( plus8 b1 b2 b3 b4  b5 b6 b7 be) bn  ))

(define-fun or3 ((b1 Bool) (b2 Bool) (b3 Bool)) Bool ( or ( or b1 b2) b3))
(define-fun one-times  ((b1 Int )) Int b1)
(define-fun two-times  ((b1 Int )) Int ( plus2 b1 b1))
(define-fun three-times  ((b1 Int )) Int ( plus3 b1 b1 b1))
(define-fun five-times  ((b1 Int )) Int ( plus5 b1 b1 b1 b1 b1 ))
(define-fun seven-times ((b1 Int )) Int ( plus7 b1 b1 b1 b1 b1 b1 b1 ))
(define-fun nine-times  ((b1 Int )) Int ( plus9 b1 b1 b1 b1 b1 b1 b1 b1 b1))
(define-fun ten-times  ((b1 Int )) Int ( plus9 b1 b1 b1 b1 b1 b1 b1 b1 ( plus2 b1 b1 )))
(define-fun minus ((b1 Int)) Int ( - 0  b1 ))

(declare-var x Int ) 
(declare-var y Int ) 
(declare-var z Int ) 

; if ( 2x -3 <= -2y +4 + z) then min(x,y,z) else max(x,y,z) endif

( constraint ( iteB  ( >= x 5 ) ( = ( ex x y ) ( plus3 (five-times x ) (three-times y) 17 ) ) ( = (ex x y ) ( plus2 (three-times x) 1 ) ) ) )
(check-synth)
    """

//; if ( 2x -3 <= -2y +4 + z) then min(x,y,z) else max(x,y,z) endif
//
//( constraint (  iteB ( <=  (  plus2 ( two-times x ) ( minus 3 ) )   ( plus3 z  ( minus ( two-times y ) )  4    ) ) (  >=  ( eq1 x y z )  x )  ( <= (eq1 x y z ) x ) ) )
//( constraint (  iteB ( <=  (  plus2 ( two-times x ) ( minus 3 ) )   ( plus3 z  ( minus ( two-times y ) )  4    ) ) (  >=  ( eq1 x y z )  y )  ( <= (eq1 x y z ) y ) ) )
//( constraint (  iteB ( <=  (  plus2 ( two-times x ) ( minus 3 ) )   ( plus3 z  ( minus ( two-times y ) )  4    ) ) (  >=  ( eq1 x y z )  z )  ( <= (eq1 x y z ) z ) ) )
//( constraint (  or3 (  =  ( eq1 x y z )  x ) (  =  ( eq1 x y z )  y ) (  =  ( eq1 x y z )  z ) ) )
    
    val pr = SyGuS16.parseSyGuS16Text(example)
    jeep.lang.Diag.println( pr )
    assertTrue( pr.isRight )
  }
  
  /////////////////////////////////

  
  def testParserImpl(rootDir: File): Unit = {
    // val root = System.getProperty("user.dir") + "/resources/sygus14"
    val files = getRecursiveListOfFiles(rootDir)

    var bag = Map.empty[String,Int]
    def add(x: Map[String,Int], s: String): Map[String,Int] =
      x + ( s -> ( 1 + x.get(s).getOrElse(0) ) )
    
    ///////////////////////////////    
    
    var mapErrorToFiles = Map.empty[String,List[File]]
      
    var numParsed = 0
    val progress = new jeep.util.ProgressDisplay( files.length )
    for( f <- files ) {
      try {
        val result = SyGuS16.parseSyGuS16File(f)
        result match {
          case Right(_) =>  numParsed += 1 
          case Left(errorMsg) => {           
            bag = add(bag,errorMsg)
            mapErrorToFiles = mapErrorToFiles.updated(errorMsg, mapErrorToFiles.getOrElse(errorMsg, Nil) :+ f )
          }
        }
      }
      catch {
        case ex: Throwable => bag = add(bag,ex.getMessage)          
      }
      
      progress.increment()
    }
    
    ///////////////////////////////
    
    if( !bag.isEmpty ) {
      jeep.lang.Diag.println(s"errors: ${bag.mkString("\n")}")
      jeep.lang.Diag.println(s"mapErrorToFiles" )
    }
      
    jeep.lang.Diag.println(s"files: ${files.length}, succesfully parsed: $numParsed")
    assertEquals( files.length, numParsed )
  }

  @Test
  def testParser: Unit = {
    {
      val root = System.getProperty("user.dir") + "/resources/sygus16"
      testParserImpl(new File(root))
    }
    {
      val root = System.getProperty("user.dir") + "/resources/sygus16new"
      testParserImpl(new File(root))
    }
  }

  @Test
  def testPBEStrings: Unit = {
    val root = System.getProperty("user.dir") + "/resources/sygus16new/PBE_Strings"
    testParserImpl(new File(root))
  }
}

// End ///////////////////////////////////////////////////////////////
