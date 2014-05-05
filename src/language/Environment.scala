package language;

import java.io.File
import java.util.concurrent.ForkJoinWorkerThread

import scala.reflect.io.Path

case class EnvConfig(
  val baseDir: String = System.getProperty("user.dir"))

case class Environment(
  val values: Environment.Values = Map(),
  val types: Environment.Types = Map(),
  val config: EnvConfig = EnvConfig()) {
  import Environment._

  def shouldFork(expr: Expression): Boolean = isParallel && expr.isInstanceOf[Application]
  def forkjoin(expressions: List[Expression]): List[Expression] = expressions.map((expression) => if (shouldFork(expression)) Left(new ExpressionTask(expression, this).fork()) else Right(Reducer.reduce(expression, this))).map((either) => either match {
    case Left(x) => x.join()
    case Right(x) => x
  })

  def ++(statements: Statements): Environment = {
    statements.foldLeft(this)((env, statement) => statement match {
      case ValueBinding(name, variables, body) => env.copy(values = env.values + (name -> (if (variables.isEmpty) Reducer.reduce(body, env) else Lambda(variables, body))))
      case TypeBinding(name, instances) => env.copy(types = env.types ++ instances.foldLeft(Map[String, String]())((map, instance) => if (map.contains(instance)) { println("duplicate type definition: " + instance); map } else map + (instance -> name)))
      case Import(name) => env ++ loadPath(format("%s/%s.par", env.config.baseDir, name))
    })
  }

  def ++(path: Path): Environment = ++(loadPath(path))
}

object Environment {
  type Values = Map[String, Expression]
  type Types = Map[String, String]
  type Statements = List[Statement]
  
  def isParallel = Thread.currentThread().isInstanceOf[ForkJoinWorkerThread]
  def loadPath(path: Path): Statements = {
    path.ifDirectory((d) => d.files.filter((f) => f.toString.endsWith(".par")).foldLeft(List[Statement]())((statements, file) => statements ::: loadPath(file.path))) match {
      case Some(list) => list
      case None => path.ifFile((f) => Parser.parseAll(Parser.program, f.bufferedReader) match {
        case Parser.Success(result, n) => result.statements
        case Parser.Failure(msg, n) => { println(format("parse error in %s: %s", f.toString, msg)); List() }
        case Parser.Error(msg, n) => { println(msg); List() }
      }) match {
        case Some(list) => list
        case None => { println("invalid path: " + path); List() }
      }
    }
  }

  def predef = List[Statement](
    ValueBinding("random", List(), Lambda(List("x", "y"), Application(Random(Variable("x"), Variable("y")), List()))))
}