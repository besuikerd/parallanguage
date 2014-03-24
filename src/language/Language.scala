package language

import scala.collection.mutable.Map

case class Program(statements:List[Statement])

sealed abstract class Statement
case class Assignment(name:String, variables:List[String], body:Expression) extends Statement

sealed abstract class Expression
case class Variable(name:String) extends Expression
case class Constructor(name:String) extends Expression
case class Constant(value:Int) extends Expression
case class Lambda(bindings:List[String], body:Expression) extends Expression
case class Application(function:Expression, arguments:List[Expression]) extends Expression
case class Match(scrutenize:Expression, cases:List[Case]) extends Expression

case class Case(constructor:String, arguments:List[String], expr:Expression)

object Reducer{
  def reduce(expression:Expression):Expression = expression match{
    case Variable(x) => Constant(5) //TODO variable bindings
    case Application(Lambda(List(), expr), x::xs) => error("too many arguments")
    case Application(Lambda(List(), expr), _) => reduce(expr)
    case Application(Lambda(binding::bindings, expr), expression::expressions) => reduce(Application(Lambda(bindings, bindArgs(expr.asInstanceOf[Application], binding, expression)), expressions))
    case app @ Application(Variable(name), List(Constant(x), Constant(y))) => binaryOps.get(name) match{
      case Some(fun) => Constant(fun(x, y))
      case _ => app
    }
    case app @ Application(f, list) => reduce(Application(f, list.map(reduce)))
    case x => x
  }
  
  def bindArgs(app:Application, name:String, expr:Expression):Application = app match{
    case Application(f, args) => Application(f, args.map((x) => x match{
      case Variable(x) if x == name => expr
      case app2 @ Application(f2, args2) => bindArgs(app2, name, expr)
      case otherwise => otherwise
    }))
  }
  
  val binaryOps = Map[String, (Int, Int) => Int](
      "plus" ->  (_ + _),
      "minus" -> (_ - _),
      "mult" -> (_ * _),
      "div" -> (_ / _)
  )
}