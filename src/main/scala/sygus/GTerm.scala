package sygus

sealed trait GTerm
case class CompositeGTerm(symbol: String, terms: List[GTerm]) extends GTerm
case class LiteralGTerm(literal: Literal) extends GTerm
case class SymbolGTerm(symbol: String) extends GTerm
case class LetGTerm(list: List[(String, SortExpr, GTerm)], term: GTerm) extends GTerm

// GenericGTerm is used as a placeholder for the other GTerms of Section 3.6, i.e. 
// Constant, Variable, InputVariable LocalVariable 
case class GenericGTerm(identifier: String, sortExpr: SortExpr) extends GTerm

// End ///////////////////////////////////////////////////////////////

