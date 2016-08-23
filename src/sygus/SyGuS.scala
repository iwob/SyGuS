package sygus

// http://www.sygus.org/SyGuS-COMP2016.html

object SyGuS {
  /* reserved words (cannot be identifiers)
set-logic,
define-sort, declare-var, declare-fun, define-fun, synth-fun, constraint, check-synth, set-options,
BitVec, Array, Int, Bool, Enum, Real, Constant, Variable, InputVariable, LocalVariable, let, true,
false
*/
/**********************
//  SyGuS ::= SetLogicCmd Cmd+ |Cmd
// SetLogicCmd ::= (set-logic Symbol)
//  Cmd ::= SortDefCmd | VarDeclCmd | FunDeclCmd | FunDefCmd | SynthFunCmd | ConstraintCmd | CheckSynthCmd | SetOptsCmd
SortExpr ::= Int | Bool | Real
| (BitVec PositiveInteger)
| (Enum (Symbol
+
))
| (Array SortExpr SortExpr)
| Symbol
SortDefCmd ::= (define-sort Symboli SortExpr)
VarDeclCmd ::= (declare-var Symboli SortExpr)
FunDeclCmd ::= (declare-fun Symboli (SortExpr ∗ ) SortExpr)

Term ::= (Symboli Term ∗)
| Literal
| Symbol
| Let Term

Let Term ::= (let ((Symbol SortExpr Term)
+
) Term)
GTerm ::= (Symbol GTerm ∗)
| Literal
| Symbol
| LetGTerm
| (Constant SortExpr)
| (Variable SortExpr)
| (InputVariable SortExpr)
| (LocalVariable SortExpr)
LetGTermi ::= (let ((Symbol SortExpr GTerm)
+
) GTerm)


SynthFunCmd ::= (synth-fun Symbol ((Symbol SortExpr)
∗
) SortExpr (NT Def
+
))
NT Def ::= (Symbol SortExpr GTerm
+
)

ConstraintCmd ::= (constraint Term)
CheckSynthCmd ::= (check-synth)
SetOptsCmd ::= (set-options ((Symbol QuotedLiteral)
+
))
**********************/
}

// End ///////////////////////////////////////////////////////////////
