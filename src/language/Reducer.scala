package language;

object Reducer {
  import language.Language._
  import language.Environment
  import language.Environment._
  import utils.Tap.any2Tap
  var cnt = 0;
  def reduce(expression: Expression, environment: Environment): Expression = {
    //    println("reducing: " + Interpreter.prettify(expression));
    cnt = cnt + 1;
    implicit def map2Environment(values: Map[String, Expression]) = environment.copy(values = values)
    implicit def environment2Map(environment: Environment) = environment.values

    expression match {
      case integer: Integer => integer
      case string: StringValue => string
      case constructor: Constructor => constructor
      case lambda: Lambda => lambda
      case Variable(x) => environment.get(x) match {
        case Some(expr) => expr
        case None => error("unknown value: " + x)
      }

      //NOTTODO flatten recursion to prevent stackoverflows
      case Match(expr, cases) => reduce(expr, environment) match {
        case Constructor(name, header) => cases.find((c) => c.constructor == name || c.isInstanceOf[DefaultCase]) match {
          case Some(c) => reduce(c.expr, environment)
          case None => error("non-exhaustive pattern")
        }
        case Application(Constructor(name, header), list) => cases.find((c) => c.constructor == name || c.isInstanceOf[DefaultCase]) match {
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
      case app @ Application(f, args) => f match {
        case Variable(name) => binaryOps.get(name) match {
          case Some(binaryOp) => args match {
            case List(e1, e2) => environment.forkjoin(List(e1, e2)) match {
              case List(Integer(x), Integer(y)) => binaryOp(x, y)
              case nonIntList => error(format("cannot apply primitive function %s with [%s]", name, nonIntList.mkString(",")))
            }
            case _ => error(format("cannot apply primitive function %s with [%s]", name, args.mkString(",")))
          }
          case _ => reduce(Application(reduce(f, environment), args), environment)
        }

        case cons @ Constructor(name, header) => if (header.evaluated) app else (environment.forkjoin(args)).tap((args) =>

          (environment.types.get(name) match {
            case Some(metricType) => environment.metrics.get(metricType) match {
              case Some(metrics) => metrics.foldLeft(cons.header.metrics)((map, metric) => map + (metric._1 -> reduce(Application(metric._2, List(Application(cons.copy(header = cons.header.copy(evaluated = true)), args))), environment)))
              case None => Map(): InstanceMetrics
            }
            case None => Map(): InstanceMetrics
          }).tap((metrics) =>
            Application(cons.copy(header = ConsHeader(metrics = metrics, evaluated = true)), args)
          )
        )

        case Random(lower, upper) => reduce(lower, environment) match {
          case Integer(i) => reduce(upper, environment) match {
            case Integer(j) => if (i > j) error(format("lower bound %d should be lower than %d in random", i, j)) else Integer(i + scala.util.Random.nextInt(j - i))
            case x => error("failed to reduce " + x + " to Integer")
          }
          case x => error("failed to reduce " + x + " to Integer")
        }

        case GetMetric(name, app) => {

          reduce(name, environment) match {

            case StringValue(name) => (reduce(app, environment).tap((app) => (app match {
              case c: Constructor => c.name
              case Application(c: Constructor, _) => c.name
              case _: Integer => "Integer"
              case unknownInstance => error("could not resolve instance type for " + unknownInstance)
            }).tap((instanceName) => {
              
              app match {
                case Application(c: Constructor, args) if (c.header.evaluated && c.header.metrics.contains(name)) =>c.header.metrics.get(name).get
                case otherwise => environment.types.get(instanceName) match {
                  case Some(metricType) => environment.metrics.get(metricType) match {
                    case Some(metrics) => metrics.get(name) match {
                      case Some(func) => {

                        val x = reduce(Application(func, List(app)), environment)

                        x
                      }
                      case None => error(format("metric %s not found for %s", name, metricType))
                    }
                    case None => { println(environment.metrics); error(format("no metrics found for type %s", metricType)) }
                  }
                  case None => error("cannot get type from " + instanceName)
                }
              }
            }
            )))

            case noStringValue => error("could not deduce metric name from " + noStringValue)

          }
        }
        case x => error(format("cannot apply %s to %s", x, args.mkString(", ")))
      }
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