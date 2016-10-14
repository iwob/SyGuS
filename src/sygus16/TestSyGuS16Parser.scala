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

  /////////////////////////////////
  
  @Test
  def testParser: Unit = {
    val root = System.getProperty("user.dir") + "/resources/sygus16"
    val files = getRecursiveListOfFiles(new File( root ) )

    var numParsed = 0
    for( f <- files ) {
      try {
        val result = SyGuS16.parseSyGuS16File(f)
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
