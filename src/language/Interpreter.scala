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
	      
	      if (parsed.successful){
	        val values = parsed.get.statements.foldLeft(Map[String, Expression]())((map, statement) => statement match{
	          case Assignment(name, List(), body) => map + (name -> body)
	          case Assignment(name, args, body) => map + (name -> Lambda(args, body))
	        })
	        values.foreach((v) => println(v))
	        Stream.continually(readLine).foreach(s => {
	        	val parsed = Parser.parseAll(Parser.expression, s)
    			if (parsed.successful) try { println(parsed.get); println(Reducer.reduce(parsed.get, values)) } catch { case e: Throwable => println(e.getMessage) } else println(parsed.toString)
	        })
	        
	      } else{
	        println(parsed.toString)
	      } 
	        
      } catch{
        case io:IOException => println(io.getMessage)
      }
    })

    

  }
}


 
