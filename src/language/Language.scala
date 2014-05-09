package language

import java.util.concurrent.RecursiveTask

import scala.collection.immutable.Map

object Language {
  
  case class Program(statements: List[Statement])

  sealed abstract class Statement
  case class ValueBinding(name: String, variables: List[String], body: Expression) extends Statement
  case class TypeBinding(name: String, instances: List[String]) extends Statement
  case class MetricDefinition(metricType: String, name: String, func: Lambda) extends Statement
  case class Import(name: String) extends Statement

  sealed abstract class Expression
  case class Variable(name: String) extends Expression
  
  type InstanceMetrics = Map[String, Expression]
  case class ConsHeader(val metrics:InstanceMetrics = Map(), val evaluated:Boolean = false)
  case class Constructor(name: String, header: ConsHeader = ConsHeader()) extends Expression
  
  case class Integer(value: Int) extends Expression
  case class StringValue(value: String) extends Expression
  case class Lambda(bindings: List[String], body: Expression) extends Expression
  case class Application(function: Expression, arguments: List[Expression] = List()) extends Expression
  case class Match(scrutenize: Expression, cases: List[AbstractCase]) extends Expression

  //specific expression constructs
  case class Random(upper: Expression, lower: Expression) extends Expression
  case class GetMetric(name:Expression, expr: Expression) extends Expression

  abstract class AbstractCase(val constructor: String, val arguments: List[String], val expr: Expression)
  case class Case(override val constructor: String, override val arguments: List[String], override val expr: Expression) extends AbstractCase(constructor, arguments, expr)
  case class DefaultCase(override val expr: Expression) extends AbstractCase("_", List(), expr)

}

