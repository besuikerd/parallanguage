package language

import java.util.concurrent.ForkJoinPool

import scala.Int
import scala.collection.immutable.Map
import scala.reflect.io.Path
import scala.util.Success
import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.JavaTokenParsers

object Interpreter {
  import Language._
  import Environment._
  
  
  def main(args: Array[String]) {
    val tempBaseDir = """C:\Users\Nicker\projects\scala\parallanguage\includes"""
    val environment = args.foldLeft(Environment(config = EnvConfig(tempBaseDir)) ++ Environment.predef)((env, arg) => env ++ arg)
    val pool = new ForkJoinPool(8)
    Stream.continually(readLine()).foreach(s => Parser.parseAll(Parser.expression, s) match {
      case Parser.Success(result, n) => try {
        
        val toReduce = new ExpressionTask(result, environment)
        val parallel = true
        val reduced = time(() => if (parallel) pool.invoke(toReduce) else toReduce.compute)
        
        println(format("[%s][%d][%dms] result: %s", if(parallel) "p" else "s", Reducer.cnt, reduced._2, prettify(reduced._1)));
        Reducer.cnt = 0;
      } catch { case e: Throwable => {e.printStackTrace(); println(e); } }
      case p:Parser.Failure => println(p)
      case p:Parser.Error => println(p)
    })
  }

  def prettify(expr: Expression): String = {
    def prettify(expr:Expression, level:Int):String =
    expr match {
      case Application(expr, args) => expr match {
        case Constructor("Conss", header) => {
          def mklist(expr: Expression): String = expr match {
            case Application(Constructor("Cons", header), x :: xs :: list) => ", " + prettify(x, level) + mklist(xs)
            case Constructor("Nils", header) => ""
            case otherwise => ", " + prettify(otherwise, level)
          }
          "[" + prettify(args.head, level) + mklist(args.last) + "]"
        }
        case Constructor("Nil", header) => "[]"
        case Constructor(name, header) => format("%s[%s](%s)", name, header, args.map((x) => prettify(x, level)).mkString(", "))

        case Variable(name) => format("%s(%s)", name, args.map((expr) => prettify(expr, level)).mkString(", "))
        case otherwise => format("(%s %s)", prettify(expr, level), args.map((expr) => prettify(expr, level)).mkString(", "))
      }

      case Integer(i) => i.toString
      case Variable(s) => s
      case Constructor(s, header) => s
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

  
}


 
