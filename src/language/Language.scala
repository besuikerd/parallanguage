package language

import java.util.concurrent.RecursiveTask

import scala.collection.immutable.Map

case class Program(statements: List[Statement])

sealed abstract class Statement
case class ValueBinding(name: String, variables: List[String], body: Expression) extends Statement
case class TypeBinding(name:String, instances:List[String]) extends Statement
case class Import(name:String) extends Statement
//case class Type()

sealed abstract class Expression
case class Variable(name: String) extends Expression
case class Constructor(name: String) extends Expression
case class Integer(value: Int) extends Expression
case class StringValue(value:String) extends Expression
case class Lambda(bindings: List[String], body: Expression) extends Expression
case class Application(function: Expression, arguments: List[Expression]) extends Expression
case class Match(scrutenize: Expression, cases: List[AbstractCase]) extends Expression
case class Random(upper: Expression, lower: Expression) extends Expression

abstract class AbstractCase(val constructor: String, val arguments: List[String], val expr: Expression)
case class Case(override val constructor: String, override val arguments: List[String], override val expr: Expression) extends AbstractCase(constructor, arguments, expr)
case class DefaultCase(override val expr: Expression) extends AbstractCase("_", List(), expr)

class ExpressionTask(val expression: Expression, val environment: Environment) extends RecursiveTask[Expression] {
  def compute() = Reducer.reduce(expression, environment)
}

object Reducer {
  var cnt = 0;
  def reduce(expression: Expression, environment: Environment): Expression = {
//    println("reducing: " + Interpreter.prettify(expression));
    cnt = cnt + 1;
    implicit def map2Environment(values: Map[String, Expression]) = Environment(values, environment.types, EnvConfig())
    implicit def environment2Map(environment: Environment) = environment.values

    expression match {
      case integer: Integer => integer
      case string:StringValue => string
      case constructor: Constructor => constructor
      case lambda: Lambda => lambda
      case Variable(x) => environment.get(x) match {
        case Some(expr) => expr
        case None => error("unknown value: " + x)
      }

      //NOTTODO flatten recursion to prevent stackoverflows
      case Match(expr, cases) => reduce(expr, environment) match {
        case Constructor(name) => cases.find((c) => c.constructor == name || c.isInstanceOf[DefaultCase]) match {
          case Some(c) => reduce(c.expr, environment)
          case None => error("non-exhaustive pattern")
        }
        case Application(Constructor(name), list) => cases.find((c) => c.constructor == name || c.isInstanceOf[DefaultCase]) match {
          case Some(c: AbstractCase) => reduce(Application(Lambda(c.arguments, c.expr), list), environment)
          case x => error("non-exhaustive pattern")
        }
        case x => error("cannot match to " + expr)
      }

      case Application(Lambda(x :: xs, _), List()) => error("too few arguments: " + (x :: xs))

      //Beta reduction
      case Application(Lambda(bindings, expr), args) if (bindings.length == args.length) => reduce(expr,
        //zip bindings with applied arguments
        bindings.zip(environment.
          //fork subexpressions
          forkjoin(
            //apply capture avoiding substitution on lambdas
            args.map((x) => if (x.isInstanceOf[Lambda]) resolveVariables(x, environment) else x)))
          //add reduced lambda arguments to the environment of the application
          .foldLeft(environment)((acc, cur) => acc + (cur._1 -> cur._2)))

      //Reduce applications
      case Application(app, args) => app match {
        case Variable(name) => binaryOps.get(name) match {
          case Some(binaryOp) => args match {
            case List(e1, e2) => environment.forkjoin(List(e1, e2)) match {
              case List(Integer(x), Integer(y)) => binaryOp(x, y)
              case nonIntList => error(format("cannot apply primitive function %s with [%s]", name, nonIntList.mkString(",")))
            }
            case _ => error(format("cannot apply primitive function %s with [%s]", name, args.mkString(",")))
          }
          case _ => reduce(Application(reduce(app, environment), args), environment)
        }
        case Constructor(name) => Application(app, args.map((x) => x match {
          case app @ Application(c: Constructor, list) => app //do not walk through recursive data structures
          case otherwise => reduce(otherwise, environment)
        }))

        case Random(lower, upper) => reduce(lower, environment) match {
          case Integer(i) => reduce(upper, environment) match {
            case Integer(j) => if (i > j) error(format("lower bound %d should be lower than %d in random", i, j)) else Integer(i + scala.util.Random.nextInt(j - i))
            case x => error("failed to reduce " + x + " to Integer")
          }
          case x => error("failed to reduce " + x + " to Integer")
        }
        case x => error(format("cannot apply %s to %s", x, args.mkString(", ")))
      };

    }
  }

  //  def substitute(lambda:Lambda, environment:Environment):Lambda = Lambda(lambda.bindings, resolveVariables(lambda.body, environment.values))

  def resolveVariables(expr: Expression, values: Map[String, Expression]): Expression = {
    val result = expr match {
      case variable @ Variable(x) => values.get(x) match {
        case Some(expr) => expr
        case None => variable
      }
      case Application(f, args) => Application(f, args.map((x) => resolveVariables(x, values)))
      case Lambda(bindings, body) => Lambda(bindings, resolveVariables(body, values -- bindings))
      case Match(matchExpr, cases) => Match(resolveVariables(matchExpr, values), cases.map((c) => c match {
        case Case(cons, consArgs, caseExpr) => Case(cons, consArgs, resolveVariables(caseExpr, values))
        case DefaultCase(defaultExpr) => DefaultCase(resolveVariables(defaultExpr, values))
      }))
      case otherwise => otherwise
    }
    result
  }

  val binaryOps = Map[String, (Int, Int) => Expression](
    "plus" -> ((x, y) => Integer(x + y)),
    "minus" -> ((x, y) => Integer(x - y)),
    "div" -> ((x, y) => Integer(x / y)),
    "mult" -> ((x, y) => Integer(x * y)),
    "mod" -> ((x, y) => Integer(x % y)),
    "pow" -> ((x, y) => Integer(Math.pow(x, y).toInt)),
    "band" -> ((x, y) => Integer(x & y)),
    "bor" -> ((x, y) => Integer(x | y)),
    "xor" -> ((x, y) => Integer(x ^ y)),
    "shiftl" -> ((x, y) => Integer(x << y)),
    "shiftr" -> ((x, y) => Integer(x >> y)),
    "ushiftr" -> ((x, y) => Integer(x >>> y)),
    "compare" -> ((x, y) => if (x > y) Constructor("GT") else if (x < y) Constructor("LT") else Constructor("EQ")))
}