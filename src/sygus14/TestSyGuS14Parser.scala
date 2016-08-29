package sygus14

import java.io._

import org.junit._
import org.junit.Assert._

class TestSyGuS14Parser {

  private def getRecursiveListOfFiles(dir: File): List[File] = {
    val these = dir.listFiles.toList
    these.filter(!_.isDirectory) ++ 
      these.filter(_.isDirectory).flatMap(getRecursiveListOfFiles)
  }
  
  /////////////////////////////////
  
  private def parseSyGuS14Text(str: String): Boolean = {
    val lines = str.split("\\r\\n|\\n|\\r").filter { x => !x.trim.startsWith(";") }
    val text = lines.mkString("\n")
    jeep.lang.Diag.println(text)
    val parser = new SyGuS14Parser    
    parser.parse(text).isDefined        
  }

  private def parseSyGuS14File(f: File): Boolean = {
    parseSyGuS14Text(scala.io.Source.fromFile(f).mkString)
  }
  
  /////////////////////////////////
  
  @Test
  def test1: Unit = {
    val text = "(set-logic LIA)"
    val parser = new SyGuS14Parser    
            
    jeep.lang.Diag.println( parser.parseAll(parser.setLogicCmd, text) )
  }

  @Test
  def test2: Unit = {
    val text = """"(set-logic LIA)
    (synth-fun max2 ((x Int) (y Int)) Int"""
    assertTrue( parseSyGuS14Text(text))
  }
  
  /////////////////////////////////

  @Test
  def testExample: Unit = {
     val path = System.getProperty("user.dir") + "/resources/example.txt"
     parseSyGuS14File(new java.io.File(path))
  }

  /////////////////////////////////
  
  @Test
  def testParser: Unit = {
    val root = System.getProperty("user.dir") + "/resources/sygus14"
    val files = getRecursiveListOfFiles(new File( root ) )

    var parsed = 0
    for( f <- files ) {
      try {
        if( parseSyGuS14File(f) ) {
          parsed += 1
        }
      }
      catch {
        case ex: Throwable => {
          jeep.lang.Diag.println(f)
          jeep.lang.Diag.println( ex.getMessage )
        }
      }
    }
    jeep.lang.Diag.println(files.length)
    jeep.lang.Diag.println(parsed)    
  }
}

// End ///////////////////////////////////////////////////////////////
