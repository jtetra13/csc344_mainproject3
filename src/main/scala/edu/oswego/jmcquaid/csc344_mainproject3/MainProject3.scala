package edu.oswego.jmcquaid.csc344_mainproject3

import scala.util.parsing.combinator._
import scala.io._

abstract class Tree

abstract class BoolExp

case class E(l: T, c: Option[String], r: Option[E]) extends Tree

case class T(l: F, c: Option[String], r: Option[T]) extends Tree()

case class F(l: Option[String], r: A) extends Tree

case class A(l: Option[String], c: Tree, r: Option[String]) extends Tree

case class C(l: Option[String], c: Option[String], r: Option[Char]) extends Tree

case class Char(c: String) extends Tree

case object True extends BoolExp

case object False extends BoolExp

case class Not(b: BoolExp) extends BoolExp

case class And(a: BoolExp, b: BoolExp) extends BoolExp

case class Or(a: BoolExp, b: BoolExp) extends BoolExp

case class Var(b: String) extends BoolExp


object MainProject3 extends Combinators {

  def convert(t: Tree): BoolExp = t match {
    case E(l, Some("||"), Some(r)) => Or(convert(l), convert(r))
    case E(l, None, None) => convert(l)
    case T(l, Some("&&"), Some(r)) => And(convert(l), convert(r))
    case T(l, None, None) => convert(l)
    case F(Some("!"), r) => Not(convert(r))
    case F(None, r) => convert(r)
    case A(Some("("), c, Some(")")) => convert(c)
    case A(None, c, None) => convert(c)
    case Char(c) => Var(c)
    case C(Some("true"), None, None) => True
    case C(None, Some("false"), None) => False
    case C(None, None, Some(Char(r))) => Var(r)
  }

  def simplify(b: BoolExp): BoolExp = b match {

    case And(l, True) => simplify(l)
    case And(True, r) => simplify(r)
    case And(l, False) => False
    case And(False, r) => False
    case And(l, r) => if (l == r) {
      l
    } else {
      And(simplify(l), simplify(r)) match {
        case And(x, True) => simplify(x)
        case And(True, y) => simplify(y)
        case And(x, False) => False
        case And(False, y) => False
        case And(x, y) => And(simplify(x), simplify(y))
      }
    }

    case Or(l, True) => True
    case Or(True, r) => True
    case Or(l, False) => simplify(l)
    case Or(False, r) => simplify(r)
    case Or(l, r) => if (l == r) {
      simplify(l)
    } else {
      Or(simplify(l), simplify(r)) match {
        case Or(x, True) => True
        case Or(True, y) => True
        case Or(x, False) => simplify(x)
        case Or(False, y) => simplify(y)
        case Or(x, y) => Or(simplify(x), simplify(y))
      }
    }

    case Not(True) => False
    case Not(False) => True
    case Not(Var(x)) => Not(Var(x))
    case Not(Not(x)) => simplify(x)
    case Not(And(x, y)) => Not(simplify(And(x, y)))
    case Not(Or(x, y)) => Not(simplify(Or(x, y)))


    case True => True
    case False => False

    case Var(c) => Var(c)

  }

  def string(b: BoolExp): String = b match {

    case And(l, r) => "(" + string(l) + " && " + string(r) + ")"
    case Or(l, r) => "(" + string(l) + " || " + string(r) + ")"
    case True => "true"
    case False => "false"
    case Not(x) => "!" + string(x)
    case Var(x) => x
  }

  def main(args: Array[String]) {
    var i = "y"
    while (i == "y") {
      println("expression? ")
      val input = readLine()
      val exp2c: Tree = parseAll(exp, input).get
      val result: String = string(simplify(convert(exp2c)))
      println("result: " + result)
      println("Another expression? ")
      i = readLine()

    }
  }
}

class Combinators extends JavaTokenParsers {
  def exp: Parser[E] = term ~ "||" ~ exp ^^ { case l ~ c ~ r => E(l, Some(c), Some(r)) } |
    term ^^ { l => E(l, None, None) }

  def term: Parser[T] = factor ~ "&&" ~ term ^^ { case l ~ c ~ r => T(l, Some(c), Some(r)) } |
    factor ^^ { l => T(l, None, None) }

  def factor: Parser[F] = "!" ~ a ^^ { case l ~ r => F(Some(l), r) } |
    a ^^ { r => F(None, r) }

  def a: Parser[A] = "(" ~ exp ~ ")" ^^ { case l ~ c ~ r => A(Some(l), c, Some(r)) } |
    c ^^ { c => A(None, c, None) }

  def c: Parser[C] = "true" ^^ { l => C(Some(l), None, None) } |
    "false" ^^ { c => C(None, Some(c), None) } |
    alpha ^^ { r => C(None, None, Some(r)) }

  def alpha: Parser[Char] = "[A-Za-z]".r ^^ { c => Char(c) }
}
