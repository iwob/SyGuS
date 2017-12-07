package sygus

sealed trait Literal
case class IntConst(value: Int) extends Literal { override def toString = value.toString }
case class RealConst(value: Double) extends Literal { override def toString = value.toString }
case class BoolConst(value: Boolean) extends Literal { override def toString = value.toString }
case class BVConst(value: List[Boolean]) extends Literal { override def toString = value.toString }
case class StringConst(value: String) extends Literal { override def toString = value.toString }
case class EnumConst(sort: String, ctor: String) extends Literal

// End ///////////////////////////////////////////////////////////////
