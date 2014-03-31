package language

import scala.collection.mutable.Map
import scala.collection.concurrent.Debug

case class Program(statements:List[Statement])

sealed abstract class Statement
case class Assignment(name:String, variables:List[String], body:Expression) extends Statement

sealed abstract class Expression
case class Variable(name:String) extends Expression
case class Constructor(name:String) extends Expression
case class Constant(value:Int) extends Expression
case class Lambda(bindings:List[String], body:Expression) extends Expression
case class Application(function:Expression, arguments:List[Expression]) extends Expression
case class Match(scrutenize:Expression, cases:List[AbstractCase]) extends Expression

abstract class AbstractCase(val constructor:String, val arguments:List[String], val expr:Expression)
case class Case(override val constructor:String, override val arguments:List[String], override val expr:Expression) extends AbstractCase(constructor, arguments, expr)
case class DefaultCase(override val expr: Expression) extends AbstractCase("_", List(), expr)

object Reducer{
  def reduce(expression:Expression, values:Map[String, Expression]):Expression = expression match{
  	case constant:Constant => constant
  	case constructor:Constructor => constructor
  	case Variable(x) => values.get(x) match{
      case Some(expr) => reduce(expr, values)
      case None => error("unknown value: " + x)
    }
    
    
    case Match(expr, cases) => reduce(expr, values) match{
      case Constructor(name) => cases.find((c) => c.constructor == name || c.isInstanceOf[DefaultCase]) match{
        case Some(c) => reduce(c.expr, values)
        case None => error("non-exhaustive pattern")
      }
      case Application(Constructor(name), list) => cases.find((c) => c.constructor == name || c.isInstanceOf[DefaultCase]) match{
	      case Some(Case(constructor, arguments, expression)) => reduce(arguments.zip(list).foldLeft(expression)((expr, bind) => bindArgs(expr, bind._1, bind._2)), values) 
	      case x => error("non-exhaustive pattern")
      		}
    }
    
    //Application of lambda function with too many arguments
    case Application(Lambda(List(), _), x::xs) => error("too many arguments")
    
    case Application(Lambda(x::xs, _), List()) => error("too few arguments: " + (x::xs))
    
    //Lambda removal
    case Application(Lambda(List(), expr), args) => {/*println(expr)*/; reduce(expr, values)}
    
    //Beta reduction
    case a @ Application(Lambda(binding::bindings, expr), expression::expressions) => {/*println("beta: " + a);*/ (reduce(Application(Lambda(bindings, bindArgs(expr, binding, expression)), expressions), values))} 
    
    //Reduce applications
    case Application(f, list) => f match{
      case Variable(name) => binaryOps.get(name) match{
        case Some(binaryOp) => list match{
          case List(Constant(x), Constant(y)) => binaryOp(x, y)
          case List(e1, e2) => reduce(Application(f, list.map((e) => reduce(e, values))), values)
          case _ => error(format("cannot apply primitive function %s with %s", name, list.mkString(",")))
        }
        case _ => values.get(name) match{
        	case Some(expr) => reduce(Application(expr, list), values)
        	case None => error("unknown value: " + name)
        }
      }
      case Constructor(name) => Application(f, list.map((x) => reduce(x, values)))
      case x => error(format("cannot apply %s to %s", x, list.mkString(", ")))
    }
  }
  
  def bindArgs(expr:Expression, name:String, binding:Expression):Expression = expr match{
    case Variable(x) if x == name => binding
  	case Application(f, args) => Application(bindArgs(f, name, binding), args.map((x) => bindArgs(x, name, binding)))
  	case Lambda(bindings, body) => Lambda(bindings, if(bindings.contains(name)) body else bindArgs(body, name, binding))
    case Match(matchExpr, cases) => Match(bindArgs(matchExpr, name, binding), cases.map((c) => c match{
        case Case(cons, consArgs, expr) => Case(cons,  consArgs, bindArgs(expr, name, binding))
        case DefaultCase(defaultExpr) => DefaultCase(bindArgs(defaultExpr, name, binding))
      }))
    case otherwise => otherwise
  }
  
  val binaryOps = Map[String, (Int, Int) => Expression](
      "plus" ->  ((x, y) => Constant(x + y)),
      "minus" -> ((x, y) => Constant(x - y)),
      "div" -> ((x, y) => Constant(x / y)),
      "mult" -> ((x, y) => Constant(x * y)),
      "compare" -> ((x, y) => if(x > y) Constructor("GT") else if(x < y) Constructor("LT") else Constructor("EQ"))
  )
}