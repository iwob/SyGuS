package sygus

import smtlib.parser._
import smtlib.parser.Commands._

object Main {

  /**
   * Example from https://github.com/regb/scala-smtlib
   */
  
  private def parseSmtLib(path: String): Unit = {
    
    val is = new java.io.FileReader( path ) // "INPUT")
    val lexer = new smtlib.lexer.Lexer(is)
    val parser = new smtlib.parser.Parser(lexer)
jeep.lang.Diag.println()    
    // The parser provides a parseCommand functions that will consume the input until the end of command:
    val script: List[Command] = {
      var cmds = new scala.collection.mutable.ListBuffer[Command]
      var cmd = parser.parseCommand
      // parseCommand returns null when the end of file is reached.
      while(cmd != null) {
        jeep.lang.Diag.println( cmd )
        cmds.append(cmd)
        cmd = parser.parseCommand        
      }
      cmds.toList
    }
jeep.lang.Diag.println()    
    // You can decompose a command using pattern matching:
    def decompose(cmd: Command) = cmd match {
      case Assert(term) => ???
      case CheckSat() => ???
      case Pop(1) => ???
    }
    
    /****
    // If you want to generate a script to feed to a solver, you can build the AST explicitly, and use the pretty printers:
    import smtlib.parser.theories.Ints._
    val x = QualifiedIdentifier(SimpleIdentifier(SSymbol("x")))
    val y = QualifiedIdentifier(SimpleIdentifier(SSymbol("y")))
    val formula = Assert(LessThan(NumeralLit(0), Plus(x, y)))
    smtlib.printer.RecursivePrinter.toString(formula) //(assert (< 0 (+ x y)))
		****/
  }

  /////////////////////////////////

  def from(input : String): List[mattmight.SExp] = {
    val parser = new mattmight.SExpParser( mattmight.SExp.streamFromIterator(input.toIterator ))
    parser.nextFile()
  }
  
  def main(args: Array[String]): Unit = {
    
    val path = System.getProperty("user.dir") + "/resources/example.txt"
   // parseSmtLib(path)
    
     jeep.lang.Diag.println( from( scala.io.Source.fromFile(path).mkString ).mkString("\n") )
     
     
  }
}

// End ///////////////////////////////////////////////////////////////
