package lib

import lib.Atom
import lib.Proposition

trait Proposition {

  def isAtom      = isInstanceOf[Atom]
  def isVariable  = isInstanceOf[Variable]
  def isConstant  = isInstanceOf[Constant]
  def isComposite = isInstanceOf[Composite]

  def toVariableOption  = Option.when(isVariable)(asInstanceOf[Variable])
  def toAtomOption      = Option.when(isAtom)(asInstanceOf[Atom])

  def vars : Set[String] = this match {
    case Variable(nam) => Set(nam)
    case Constant(nam) => Set()
    case Composite(nam, on) => on.foldLeft(Set())((set, prop) => set ++ prop.vars)
  }

}

trait Atom extends Proposition
case class Variable(name : String) extends Atom
case class Constant(name : String) extends Atom

case class Composite(name : String, on : List[Proposition]) extends Proposition:
  def head = on.head
  def body = on.tail


