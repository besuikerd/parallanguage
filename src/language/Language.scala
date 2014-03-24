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
    
    //Application of lambda function with too many arguments
    case Application(Lambda(List(), expr), x::xs) => error("too many arguments")
    
    //Lambda removal
    case Application(Lambda(List(), expr), _) => reduce(expr)
    
    //Beta reduction
    case Application(Lambda(binding::bindings, expr), expression::expressions) => expr match{
      case app:Application => reduce(Application(Lambda(bindings, bindArgs(app, binding, expression)), expressions))
      case x => reduce(x)
    } 
    
    //Reduce primitive binary operations
    case app @ Application(Variable(name), List(Constant(x), Constant(y))) => binaryOps.get(name) match{
      case Some(fun) => fun(x, y)
      case _ => app
    }
    
    //Reduce applications
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
  
  val binaryOps = Map[String, (Int, Int) => Expression](
      "plus" ->  ((x, y) => Constant(x + y)),
      "minus" -> ((x, y) => Constant(x - y)),
      "div" -> ((x, y) => Constant(x / y)),
      "mult" -> ((x, y) => Constant(x * y)),
      "compare" -> ((x, y) => if(x > y) Constructor("GT") else if(x < y) Constructor("LT") else Constructor("EQ"))
  )
}