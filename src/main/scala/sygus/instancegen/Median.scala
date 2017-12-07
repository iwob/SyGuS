package sygus.instancegen

object Median {
  import sygus._
  import sygus14._

  /////////////////////////////////
  
  final case class PermutationIterator(n: Int) extends Iterator[Array[Int]] {
    private val impl = new jeep.math.PermutationGenerator(n)
  
    override def hasNext: Boolean = impl.hasNext
    override def next(): Array[Int] = impl.next()
  }  

  /////////////////////////////////

  implicit def Int2Boolean(x: Int) = !(x == 0)
  implicit def Boolean2Int(b: Boolean) = if( b ) 1 else 0
  
  sealed trait Expr {
    def eval: Int 
  }

  sealed case class Lit(value: Int) extends Expr {
    override def eval: Int = value
  }

  case class Implies(a: Expr, b: Expr) extends Expr {
    override def eval: Int = if( a.eval ) b.eval else 1   
  }

  case class And(x: Expr, y: Expr) extends Expr {
    override def eval: Int = x.eval && y.eval    
  }

  case class Or(x: Expr, y: Expr) extends Expr {
    override def eval: Int = x.eval || y.eval    
  }

  case class Not(x: Expr) extends Expr {
    override def eval: Int = !x.eval    
  }

  case class GT(v1: Int, v2: Int) extends Expr {
    override def eval: Int = v1 > v2    
  }

  case class LE(v1: Int, v2: Int) extends Expr {
    override def eval: Int = v1 <= v2    
  }

  case class EQ(e1: Expr, e2: Expr) extends Expr {
    override def eval: Int = e1 == e2    
  }

  case class Minus(v1: Int, v2: Int) extends Expr {
    override def eval: Int = v1 - v2    
  }
  
  /////////////////////////////////

  def median3(a: Int, b: Int, c: Int): Int = {
    if (a > b) {
      if (b > c) b else { if (a > c) c else a }
    } else {
      if (a > c) a else { if (b > c) c else b }
    }
  }

  def median(s: Seq[Int]): Int = {
    val (lower, upper) = s.sortWith(_<_).splitAt(s.size / 2)
    if (s.size % 2 == 0) (lower.last + upper.head) / 2 else upper.head
  }  
  
  def verifyMedian: Unit = {  

    def c1(a: Int, b: Int, c: Int): Expr =
      Implies( GT(a,b), Implies( GT(b,c), Lit(b) ) )
    def c2(a: Int, b: Int, c: Int): Expr =    
      Implies( GT(a,b), Implies( LE(b,c), Implies( GT( a, c ), Lit(c )) ) )    
    def c3(a: Int, b: Int, c: Int): Expr =    
      Implies( GT(a,b), Implies( LE(b,c), Implies( LE( a, c ), Lit( a )) ) )    

    def c4(a: Int, b: Int, c: Int): Expr =
      Implies( LE(a,b), Implies( GT(a,c), Lit(a) ) )
    def c5(a: Int, b: Int, c: Int): Expr =    
      Implies( LE(a,b), Implies( LE(a,c), Implies( GT( b, c ), Lit( c )) ) )    
    def c6(a: Int, b: Int, c: Int): Expr =    
      Implies( LE(a,b), Implies( LE(a,c), Implies( LE( b, c ), Lit( b )) ) )    

    val spec = List(c1 _,c2 _,c3 _,c4 _,c5 _,c6 _)

    for( perm <- PermutationIterator(3) ) {
      val m3 = median3(perm(0), perm(1), perm(2) )
      val ms = median(perm)
      println( perm.mkString( "[", ",", "]"), m3, ms )
      assert( m3 == ms )
      println( spec.forall { c => c( perm(0), perm(1), perm(2) ).eval == ms } )
    }
  }
  
  /////////////////////////////////
  
  def toBinary(value: Int, length: Int): List[Boolean] =
    (0 until length).map { i =>
      (1 << i & value) != 0
    }.toList

  def verifySortedAscending4: Unit = {
    def c1(a: Int, b: Int, c: Int, d: Int): Expr =
      Implies( And( LE(a, b), And( LE(b, c), LE(c, d ) ) ), Lit(1) )
    def c2(a: Int, b: Int, c: Int, d: Int): Expr =
      Implies( Not( And( LE(a, b), And( LE(b, c), LE(c, d ) ) ) ), Lit(0) )
    
    for(i <- 0 until 16) {      
      val b = toBinary(i, length=4)
      println( b.map { Boolean2Int }.mkString("[",",","]") + " -> " +
        // c1( b(0), b(1), b(2), b(3) ).eval, 
        c2( b(0), b(1), b(2), b(3) ).eval )
    }
  }

//(constraint (=> (and (= (- b a) (- c b)) (= (- c b) (- d c)) )
//                (= (arithmeticseries4 a b c d) 1)))
//(constraint (=> (not (and (= (- b a) (- c b)) (= (- c b) (- d c)) ))
//                (= (arithmeticseries4 a b c d) 0)))    

  def verifyArithmeticSeries4: Unit = {
    def c1(a: Int, b: Int, c: Int, d: Int): Expr =
      Implies( And( EQ( Minus(b, a), Minus(c, b)), EQ( Minus(c, b), Minus(d, c)) ), Lit(1) )
          
    def c2(a: Int, b: Int, c: Int, d: Int): Expr =
      Implies( Not( And( EQ( Minus(b, a), Minus(c, b)), EQ( Minus(c, b), Minus(d, c)) ) ), Lit(0) )
    
    for(i <- 0 until 16) {      
      val b = toBinary(i, length=4)
      println( b.map { Boolean2Int }.mkString("[",",","]") + " -> " + 
        // c1( b(0), b(1), b(2), b(3) ).eval, 
        c2( b(0), b(1), b(2), b(3) ).eval )
    }
  }
    
  /////////////////////////////////  
  
  def main(args: Array[String]): Unit = {
    // verifyMedian
    verifySortedAscending4
    println( "**************" )
    verifyArithmeticSeries4
  }
}

///////////////////////////////////

/***************************************

object GenerateSyGuS {

  import sygus._
  import sygus14._

  /////////////////////////////////
  
  val plusGTerm = CompositeGTerm("+", List(SymbolGTerm("Start"),SymbolGTerm("Start")))
  val minusGTerm = CompositeGTerm("-", List(SymbolGTerm("Start"),SymbolGTerm("Start")))
  val iteGTerm = CompositeGTerm("ite", List(SymbolGTerm("StartBool"),SymbolGTerm("Start"),SymbolGTerm("Start")))
  
  val andGTerm = CompositeGTerm("and", List(SymbolGTerm("StartBool"),SymbolGTerm("StartBool")))
  val orGTerm = CompositeGTerm("or", List(SymbolGTerm("StartBool"),SymbolGTerm("StartBool")))  
  val notGTerm = CompositeGTerm("not", List(SymbolGTerm("StartBool")))

  val leGTerm = CompositeGTerm("<=", List(SymbolGTerm("Start"),SymbolGTerm("Start"))) 
  val eqGTerm = CompositeGTerm("=", List( SymbolGTerm("Start"), SymbolGTerm("Start"))) 
  val geGTerm = CompositeGTerm(">=", List(SymbolGTerm("Start"),SymbolGTerm("Start")))
  
  /////////////////////////////////
  
  def max2: SyGuS14 = {

    val sygus = SyGuS14(Some(SyGuS14.SetLogicCmd(SetLogicTheory.LIA)),
      List(
        SynthFunCmd14("max2", List(("x",IntSortExpr()),("y",IntSortExpr())),
          IntSortExpr(),
          List(
            NTDef("Start", IntSortExpr(),
              List(
                SymbolGTerm("x"),SymbolGTerm("y"), 
                LiteralGTerm(IntConst(0)), LiteralGTerm(IntConst(1)), 
                plusGTerm, minusGTerm, iteGTerm
              )
            ), 
            NTDef("StartBool",BoolSortExpr(),
              List( andGTerm, orGTerm, notGTerm, leGTerm, eqGTerm, geGTerm )
            )
          )
        ), 
        VarDeclCmd("x",IntSortExpr()), 
        VarDeclCmd("y",IntSortExpr()), 
        ConstraintCmd(
          CompositeTerm(">=", List( CompositeTerm("max2", List( SymbolTerm("x"), SymbolTerm("y") )),SymbolTerm("x")))
        ), 
        ConstraintCmd(
          CompositeTerm(">=", List( CompositeTerm("max2", List( SymbolTerm("x"), SymbolTerm("y"))),SymbolTerm("y")))
        ), 
        ConstraintCmd(
          CompositeTerm("or",
            List(
              CompositeTerm("=",
                List(SymbolTerm("x"),CompositeTerm("max2",List(SymbolTerm("x"),SymbolTerm("y"))))
              ), 
              CompositeTerm("=",
                List(SymbolTerm("y"),CompositeTerm("max2",List(SymbolTerm("x"),SymbolTerm("y"))))
              )
            )
          )
        ), 
        CheckSynthCmd()
      )
    )
    
    ///////////////////////////////
    
    sygus
  }
}
***************************************/

///////////////////////////////////

object Check2 {
  
  import sygus._
  import sygus14._
  
  def main( args: Array[String]): Unit = {
    println( new java.util.Date() )

    val medianStr = """
      (set-logic LIA)

      (synth-fun median3 ((a Int) (b Int) (c Int)) Int

      ((Start Int (a b c 0 1
             (+ Start Start)
             (- Start Start)
             (ite StartBool Start Start)))

       (StartBool Bool ((and StartBool StartBool)
                  (or StartBool StartBool)
                  (=> StartBool StartBool)
                  (not StartBool)
                  (<= Start Start)
                  (= Start Start)
                  (>= Start Start)))))

      (declare-var a Int)
      (declare-var b Int)
      (declare-var c Int)

      (constraint (=> (> a b) (=> (> b c) (= (median3 a b c) b) ) ))
      (constraint (=> (> a b) (=> (<= b c) (=> (> a c) (= (median3 a b c) c) ) ) ) )
      (constraint (=> (> a b) (=> (<= b c) (=> (<= a c ) (= (median3 a b c) a) ) ) ) )
      (constraint (=> (<= a b) (=> (> a c) (= (median3 a b c) a) ) ) )
      (constraint (=> (<= a b) (=> (<= a c) (=> (> b c) (= (median3 a b c) c) ) ) ) )
      (constraint (=> (<= a b) (=> (<= a c) (=> (<= b c ) (= (median3 a b c) b) ) ) ) )
      
      (check-synth)
      """

    println( SyGuS14( medianStr ) )

    ///////////////////////////////

    val countPositive2Str = """
      (set-logic LIA)

      (synth-fun countpositive2 ((a Int) (b Int)) Int

      ((Start Int (a b 0 1
             (+ Start Start)
             (- Start Start)
             (ite StartBool Start Start)))

       (StartBool Bool ((and StartBool StartBool)
                  (or StartBool StartBool)
                  (=> StartBool StartBool)
                  (not StartBool)
                  (<= Start Start)
                  (= Start Start)
                  (>= Start Start)))))

      (declare-var a Int)
      (declare-var b Int)

      (constraint (=> (and (<= a 0) (<= b 0)) (= (countpositive2 a b) 0)))
      (constraint (=> (and (> a 0)  (<= b 0)) (= (countpositive2 a b) 1)))
      (constraint (=> (and (<= a 0) (> b 0))  (= (countpositive2 a b) 1)))
      (constraint (=> (and (> a 0)  (> b 0))  (= (countpositive2 a b) 2)))      
      (check-synth)
      """
    
    ///////////////////////////////
    
    println( SyGuS14( countPositive2Str ) )    
    
    val countPositive3Str = """
      (set-logic LIA)

      (synth-fun countpositive3 ((a Int) (b Int) (c Int)) Int

      ((Start Int (a b c 0 1
             (+ Start Start)
             (- Start Start)
             (ite StartBool Start Start)))

       (StartBool Bool ((and StartBool StartBool)
                  (or StartBool StartBool)
                  (=> StartBool StartBool)
                  (not StartBool)
                  (<= Start Start)
                  (= Start Start)
                  (>= Start Start)))))

      (declare-var a Int)
      (declare-var b Int)
      (declare-var c Int)

      (constraint (=> (and (<= a 0) (and (<= b 0) (<= c 0))) (= (countpositive3 a b c) 0)))
      (constraint (=> (and (<= a 0) (and (<= b 0) (> c 0)))  (= (countpositive3 a b c) 1))) 
      (constraint (=> (and (<= a 0) (and (> b 0)  (<= c 0))) (= (countpositive3 a b c) 1)))
      (constraint (=> (and (> a 0)  (and (<= b 0) (<= c 0))) (= (countpositive3 a b c) 1)))
      (constraint (=> (and (> a 0) (and (> b 0) (<= c 0))) (= (countpositive3 a b c) 2)))
      (constraint (=> (and (> a 0) (and (<= b 0) (> c 0))) (= (countpositive3 a b c) 2)))
      (constraint (=> (and (<= a 0) (and (> b 0) (> c 0))) (= (countpositive3 a b c) 2)))
      (constraint (=> (and (> a 0) (and (> b 0) (> c 0))) (= (countpositive3 a b c) 3)))      
      (check-synth)
      """
    
    println( SyGuS14( countPositive3Str ) )    

    ///////////////////////////////
    
    val sortedAscending4Str = """
      (set-logic LIA)

      (synth-fun sortedascending4 ((a Int) (b Int) (c Int) (d Int)) Int

      ((Start Int (a b c d 0 1
             (+ Start Start)
             (- Start Start)
             (ite StartBool Start Start)))

       (StartBool Bool ((and StartBool StartBool)
                  (or StartBool StartBool)
                  (=> StartBool StartBool)
                  (not StartBool)
                  (<= Start Start)
                  (= Start Start)
                  (>= Start Start)))))

      (declare-var a Int)
      (declare-var b Int)
      (declare-var c Int)
      (declare-var d Int)
      
      (constraint (=> (and (< a b) (and (< b c) (< c d))) (= (sortedascending4 a b c d) 1)))
      (constraint (=> (and (< a b) (and (< b c) (>= c d))) (= (sortedascending4 a b c d) 0)))
      (constraint (=> (and (< a b) (and (>= b c) (< c d))) (= (sortedascending4 a b c d) 0)))
      (constraint (=> (and (< a b) (and (>= b c) (>= c d))) (= (sortedascending4 a b c d) 0)))
      (constraint (=> (and (>= a b) (and (< b c) (< c d))) (= (sortedascending4 a b c d) 0)))
      (constraint (=> (and (>= a b) (and (< b c) (>= c d))) (= (sortedascending4 a b c d) 0)))      
      (constraint (=> (and (>= a b) (and (>= b c) (< c d))) (= (sortedascending4 a b c d) 0)))      
      (constraint (=> (and (>= a b) (and (>= b c) (>= c d))) (= (sortedascending4 a b c d) 0)))            
      (check-synth)
      """
    
    println( SyGuS14( sortedAscending4Str ) )
    
    ///////////////////////////////
    
    val arithmeticSeries3Str = """
      (set-logic LIA)

      (synth-fun arithmeticseries3 ((a Int) (b Int) (c Int)) Int

      ((Start Int (a b c 0 1
             (+ Start Start)
             (- Start Start)
             (ite StartBool Start Start)))

       (StartBool Bool ((and StartBool StartBool)
                  (or StartBool StartBool)
                  (=> StartBool StartBool)
                  (not StartBool)
                  (<= Start Start)
                  (= Start Start)
                  (>= Start Start)))))

      (declare-var a Int)
      (declare-var b Int)
      (declare-var c Int)
      
      (constraint (=> (and (= (- b a) (- c b)) (and (= (- b a) (- d c) ))) (= (arithmeticseries3 a b c) 1)))
      (constraint (=> (and (not (= (- b a) (- c b))) (and (= (- b a) (- d c) ))) (= (arithmeticseries3 a b c) 0)))
      (constraint (=> (and (= (- b a) (- c b)) (and (not (= (- b a) (- d c) )))) (= (arithmeticseries3 a b c) 0)))
      (constraint (=> (and (not (= (- b a) (- c b))) (and (not (= (- b a) (- d c) )))) (= (arithmeticseries3 a b c) 0)))      
      (check-synth)
      """
    
    println( SyGuS14( arithmeticSeries3Str ) )
    
    ///////////////////////////////

    val range3Str = """
      (set-logic LIA)

      (synth-fun range3 ((a Int) (b Int) (c Int)) Int

      ((Start Int (a b c 0 1
             (+ Start Start)
             (- Start Start)
             (ite StartBool Start Start)))

       (StartBool Bool ((and StartBool StartBool)
                  (or StartBool StartBool)
                  (=> StartBool StartBool)
                  (not StartBool)
                  (<= Start Start)
                  (= Start Start)
                  (>= Start Start)))))

      (declare-var a Int)
      (declare-var b Int)
      (declare-var c Int)
      
      (constraint (=> (and (< a b)  (< b c))  (= (range3 a b c) (- c a))))
      (constraint (=> (and (< a b)  (>= b c)) (= (range3 a b c) (- b a))))
      (constraint (=> (and (>= a b) (< b c))  (= (range3 a b c) (- a b))))
      (constraint (=> (and (>= a b) (>= b c)) (= (range3 a b c) (- a c))))      
      (check-synth)
      """
    
    println( SyGuS14( range3Str ) )
    
  }  
}

// End ///////////////////////////////////////////////////////////////
