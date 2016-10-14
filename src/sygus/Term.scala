package sygus

sealed trait Term
case class CompositeTerm(symbol: String, terms: List[Term]) extends Term
case class LiteralTerm(literal: Literal) extends Term
case class SymbolTerm(symbol: String) extends Term
case class LetTerm(list: List[(String, SortExpr, Term)], term: Term) extends Term

// End ///////////////////////////////////////////////////////////////

