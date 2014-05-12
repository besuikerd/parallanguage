package language;

import java.io.File
import java.util.concurrent.ForkJoinWorkerThread
import scala.reflect.io.Path
import java.util.concurrent.RecursiveTask

case class Environment(
  val values: Environment.Values = Map(),
  val types: Environment.Types = Map(),
  val metrics: Environment.Metrics = Map(),
  val config: Environment.EnvConfig = Environment.EnvConfig()) {

  import Environment._
  import Language._
  import Reducer._
  def shouldFork(expr: Expression): Boolean = isParallel && expr.isInstanceOf[Application]
  def forkjoin(expressions: List[Expression]): List[Expression] = expressions.map((expression) => if (shouldFork(expression)) Left(new ExpressionTask(expression, this).fork()) else Right(Reducer.reduce(expression, this))).map((either) => either match {
    case Left(x) => x.join()
    case Right(x) => x
  })

  def ++(statements: Statements): Environment = {
    statements.foldLeft(this)((env, statement) => statement match {
      case ValueBinding(name, variables, body) => env.copy(values = env.values + (name -> (if (variables.isEmpty) Reducer.reduce(body, env) else Lambda(variables, body))))
      case TypeBinding(name, instances) => env.copy(types = (env.types ++ instances.foldLeft(Map[Instance, Type]())((map, instance) => if (map.contains(instance)) { println("duplicate type definition: " + instance); map } else map + (instance -> name))))
      case Import(name) => env ++ loadPath(format("%s/%s.par", env.config.baseDir, name))
      case MetricDefinition(metricType, name, func) => env.copy(metrics = env.metrics +
        (metricType -> ((env.metrics.get(metricType) match {
          case Some(typeMetrics) => typeMetrics
          case None => Map[String, Lambda]()
        }) + (name -> func))))
    })
  }

  def ++(path: Path): Environment = ++(loadPath(path))
}

object Environment {
  import Language._
  import Reducer._

  type Name = String
  type Values = Map[Name, Expression]
  type Type = String
  type Instance = String
  type Types = Map[Instance, Type]
  type Metrics = Map[Type, TypeMetrics]
  type TypeMetrics = Map[Name, Lambda]
  type Statements = List[Statement]

  case class EnvConfig(
    val baseDir: String = System.getProperty("user.dir"))

  class ExpressionTask(val expression: Language.Expression, val environment: Environment) extends RecursiveTask[Language.Expression] {
    def compute() = Reducer.reduce(expression, environment)
  }

  def isParallel = Thread.currentThread().isInstanceOf[ForkJoinWorkerThread]
  def loadPath(path: Path): Statements = {
    path.ifDirectory((d) => d.files.filter((f) => f.toString.endsWith(".par")).foldLeft(List[Statement]())((statements, file) => statements ::: loadPath(file.path))) match {
      case Some(list) => list
      case None => path.ifFile((f) => Parser.parseAll(Parser.program, f.bufferedReader) match {
        case Parser.Success(result, n) => result.statements
        case p: Parser.Failure => { println(format("parse failure in %s: %s", f.toString, p)); List() }
        case p: Parser.Error => { println(format("parse error in %s: %s", f.toString, p)); List() }
      }) match {
        case Some(list) => list
        case None => { println("invalid path: " + path); List() }
      }
    }
  }

  def predef = List[Statement](
    ValueBinding("random", List(), Lambda(List("x", "y"), Application(Random(Variable("x"), Variable("y")))))
    ,ValueBinding("metric", List(), Lambda(List("name", "x"), Application(GetMetric(Variable("name"), Variable("x")))))

    ,TypeBinding("Integer", List("Integer"))
    ,TypeBinding("String", List("String"))

  )
}