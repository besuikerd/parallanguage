package language

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.Success
import scala.collection.immutable.Map
import scala.collection.immutable.HashMap
import scala.io.Source
import java.io.IOException
import java.io.File
import scala.reflect.io.Path
import scala.Int
import scala.collection.immutable.HashSet
import java.util.concurrent.ForkJoinPool

object Interpreter {
  def main(args: Array[String]) {
    val environment = Environment(args.foldLeft(Map[String, Expression]())((map, arg) => map ++ loadFile(arg)) ++ Map(
      "random" -> Lambda(List("x", "y"), Application(Random(Variable("x"), Variable("y")), List()))))
    val pool = new ForkJoinPool(8)
    Stream.continually(readLine).foreach(s => Parser.parseAll(Parser.expression, s) match {
      case Parser.Success(result, n) => try {
        println(prettify(result));
        val toReduce = new ExpressionTask(result, environment)
        val parallel = true
        val reduced = time(() => if (parallel) pool.invoke(toReduce) else toReduce.compute)
        
        
        println(format("[%s][%d][%dms] result: %s", if(parallel) "p" else "s", Reducer.cnt, reduced._2, prettify(reduced._1)));
        Reducer.cnt = 0;
      } catch { case e: Throwable => { println(e); e.printStackTrace() } }
      case Parser.Failure(msg, n) => println(msg)
      case Parser.Error(msg, n) => println(msg)
    })
  }

  def prettify(expr: Expression): String = {
    def prettify(expr:Expression, level:Int):String =
    expr match {
      case Application(expr, args) => expr match {
        case Constructor("Cons") => {
          def mklist(expr: Expression): String = expr match {
            case Application(Constructor("Cons"), x :: xs :: list) => ", " + prettify(x, level) + mklist(xs)
            case Constructor("Empty") => ""
            case otherwise => ", " + prettify(otherwise, level)
          }
          "[" + prettify(args.head, level) + mklist(args.last) + "]"
        }
        case Constructor("Empty") => "[]"
        case Constructor(name) => format("%s(%s)", name, args.map((x) => prettify(x, level)).mkString(", "))

        case Variable(name) => format("%s(%s)", name, args.map((expr) => prettify(expr, level)).mkString(", "))
        case otherwise => format("(%s %s)", prettify(expr, level), args.map((expr) => prettify(expr, level)).mkString(", "))
      }

      case Integer(i) => i.toString
      case Variable(s) => s
      case Constructor(s) => s
      case Match(expr, cases) => {
        "match " + prettify(expr, level) + "\n" +
          cases.foldLeft("")((acc, cur) => format("%s%s case %s %s => %s \n", acc, "\t" * level, cur.constructor, cur.arguments.mkString(" "), prettify(cur.expr, level + 1)))
      }
      case Lambda(bindings, expr) => format("\\%s -> %s", bindings.mkString(" "), prettify(expr, level))
      case otherwise => otherwise.toString
    }
    prettify(expr, 0)
  }
  
  def time[R](block: () => R): Tuple2[R, Long] = {
	    val t = System.currentTimeMillis
	    return Tuple2(block(), System.currentTimeMillis - t)
  }

  def loadFile(path: Path): Map[String, Expression] = {
    path.ifDirectory((d) => d.files.filter((f) => f.toString.endsWith(".par")).foldLeft(Map[String, Expression]())((map, file) => map ++ loadFile(file.path))) match {
      case Some(map) => map
      case None => path.ifFile((f) => Parser.parseAll(Parser.program, f.bufferedReader) match {
        case Parser.Success(result, n) => result.statements.foldLeft(Map[String, Expression]())((map, statement) => statement match {
          case Assignment(name, List(), body) => map + (name -> body)
          case Assignment(name, args, body) => map + (name -> Lambda(args, body))
        })
        case Parser.Failure(msg, n) => { println(format("parse error in %s: %s", f.toString, msg)); Map[String, Expression]() }
        case Parser.Error(msg, n) => { println(msg); Map[String, Expression]() }
      }) match {
        case Some(map) => map
        case None => { println("invalid path: " + path); Map() }
      }

    }
  }
}


 
