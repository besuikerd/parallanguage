package language

import java.util.concurrent.ForkJoinWorkerThread
import java.util.concurrent.RecursiveTask

import scala.collection.immutable.Map

case class Program(statements: List[Statement])

sealed abstract class Statement
case class Assignment(name: String, variables: List[String], body: Expression) extends Statement

sealed abstract class Expression
case class Variable(name: String) extends Expression
case class Constructor(name: String) extends Expression
case class Integer(value: Int) extends Expression
case class Lambda(bindings: List[String], body: Expression) extends Expression
case class Application(function: Expression, arguments: List[Expression]) extends Expression
case class Match(scrutenize: Expression, cases: List[AbstractCase]) extends Expression
case class Random(upper: Expression, lower: Expression) extends Expression

abstract class AbstractCase(val constructor: String, val arguments: List[String], val expr: Expression)
case class Case(override val constructor: String, override val arguments: List[String], override val expr: Expression) extends AbstractCase(constructor, arguments, expr)
case class DefaultCase(override val expr: Expression) extends AbstractCase("_", List(), expr)

case class Environment(val values:Map[String, Expression]) {
  val isParallel = Thread.currentThread().isInstanceOf[ForkJoinWorkerThread]
  def shouldFork(expr: Expression): Boolean = isParallel
  def forkjoin(expressions: List[Expression]): List[Expression] = expressions.map((expression) => if (shouldFork(expression)) Left(new ExpressionTask(expression, this).fork()) else Right(Reducer.reduce(expression, this))).map((either) => either match {
    case Left(x) => x.join()
    case Right(x) => x
  })
}
class ExpressionTask(val expression: Expression, val environment: Environment) extends RecursiveTask[Expression] {
  def compute() = Reducer.reduce(expression, environment)
}

//TODO environment vs binding
//TODO objects are not dereferenced, memory heap grows enormously
object Reducer {
  var cnt = 0;
  def reduce(expression: Expression, environment: Environment): Expression = {
//        println("reducing: " + Interpreter.prettify(expression));
    cnt = cnt + 1;
    implicit def Map2Environment(values:Map[String, Expression]) = Environment(values)
    implicit def Environment2Map(environment:Environment) = environment.values
    
    expression match {
      case integer: Integer => integer
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
          bindings.zip(environment.forkjoin(args)).foldLeft(environment)((acc, cur) => acc + (cur._1 -> cur._2))
          
          )
//    		  args.map((x) => reduce(x, environment)
      //Reduce applications
      case Application(app, args) => app match {
        case Variable(name) => binaryOps.get(name) match {
          case Some(binaryOp) => args match {
            case List(e1, e2) => List(reduce(e1, environment), reduce(e2, environment)) match {
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

  def resolveVariables(expr: Expression, values: Map[String, Expression]): Expression = {
    println("resolving: " + expr); val result = expr match {
      case variable @ Variable(x) => values.get(x) match {
        case Some(expr) => { println("found: " + x + ": " + Interpreter.prettify(expr)); expr }
        case None => variable
      }
      case Application(f, args) => Application(f, args.map((x) => resolveVariables(x, values)))
      case Lambda(bindings, body) => { println(format("%s: %s", expr, (values).contains("x"))); Lambda(bindings, resolveVariables(body, values -- bindings)) }
      case Match(matchExpr, cases) => Match(resolveVariables(matchExpr, values), cases.map((c) => c match {
        case Case(cons, consArgs, caseExpr) => Case(cons, consArgs, resolveVariables(caseExpr, values))
        case DefaultCase(defaultExpr) => DefaultCase(resolveVariables(defaultExpr, values))
      }))
      case otherwise => otherwise
    }
    println("resolved: " + expr + " to: " + result)
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