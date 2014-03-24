package language

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.Success
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.io.Source
import java.io.IOException

object Interpreter {
  def main(args: Array[String]) {

    args.foreach((s) => {
      try{
	      val file = Source.fromFile(s).mkString
	      val parsed = Parser.parseAll(Parser.program, file)
	      if (parsed.successful) println(parsed.get.statements.foreach(s => println(s))) else println(parsed.toString)
      } catch{
        case io:IOException => println(io.getMessage)
      }
    })

    //	  

    testReduce

    Stream.continually(readLine).foreach(s => {
      val parsed = Parser.parseAll(Parser.expression, s)
      if (parsed.successful) try { println(parsed.get); println(Reducer.reduce(parsed.get)) } catch { case e: Throwable => println(e.getMessage) } else println(parsed.toString)
    })

  }

  def testReduce = {
    // \x y z -> x `plus` (y `plus` z)
    val lambda = Lambda(List("x", "y", "z"), Application(Variable("plus"), List(Variable("x"), Application(Variable("mult"), List(Variable("y"), Variable("z"))))))
    
    // (\x y z -> (x `plus` (y `plus` z)) 1 2 3)
    val application = Application(lambda, List(Constant(1), Constant(2), Constant(3)))
    
    println(application)
    println(Reducer.reduce(application))
  }
}


 
