package sygus

sealed trait SortExpr {
  def name: String
}
case class IntSortExpr() extends SortExpr { val name = "Int" }
case class BoolSortExpr() extends SortExpr { val name = "Bool" }
case class RealSortExpr() extends SortExpr { val name = "Real" }
case class BitVecSortExpr(n: Int) extends SortExpr {
  require(n > 0)
  val name = s"BitVec[$n]"
}

case class EnumSortExpr(symbols: List[String]) extends SortExpr {
  val name = s"Enum(${symbols.mkString(",")})"
}
case class ArraySortExpr(sortExpr1: SortExpr, sortExpr2: SortExpr) extends SortExpr {
  val name = s"Array[$sortExpr1,$sortExpr2]"
}
case class SymbolSortExpr(symbol: String) extends SortExpr { val name = symbol }

// SyGuS16:
case class StringSortExpr() extends SortExpr { val name = "String" }

  // End /////////////////////////////////////////////////////////////
