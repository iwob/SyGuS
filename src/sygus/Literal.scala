package sygus

sealed trait Literal
case class IntConst(value: Int) extends Literal
case class RealConst(value: Double) extends Literal
case class BoolConst(value: Boolean) extends Literal
case class BVConst(value: List[Boolean]) extends Literal
case class EnumConst(sort: String, ctor: String) extends Literal

// End ///////////////////////////////////////////////////////////////
