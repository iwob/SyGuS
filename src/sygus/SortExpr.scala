package sygus

sealed trait SortExpr
case class IntSortExpr() extends SortExpr
case class BoolSortExpr() extends SortExpr
case class RealSortExpr() extends SortExpr
case class BitVecSortExpr(n: Int) extends SortExpr {
  require(n > 0)
}

case class EnumSortExpr(symbols: List[String]) extends SortExpr
case class ArraySortExpr(sortExpr1: SortExpr, sortExpr2: SortExpr) extends SortExpr
case class SymbolSortExpr(symbol: String) extends SortExpr

  // End /////////////////////////////////////////////////////////////
