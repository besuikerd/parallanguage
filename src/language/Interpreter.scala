package language

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.Success
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.io.Source
import java.io.IOException
import java.io.File
import scala.reflect.io.Path

object Interpreter {
  def main(args: Array[String]) {

    val values = args.foldLeft(Map[String, Expression]())((map, arg) => map ++ loadFile(arg))
    Stream.continually(readLine).foreach(s => Parser.parseAll(Parser.expression, s) match{
      case Parser.Success(result, n) => try{println(result); println(Reducer.reduce(result, values))} catch{case e:Throwable => println(e.getMessage)}
      case Parser.Failure(msg, n) => println(msg)
      case Parser.Error(msg, n) => println(msg)
    })

  }
  
  def loadFile(path:Path):Map[String, Expression] = {
    path.ifDirectory((d) => d.files.filter((f) => f.toString.endsWith(".par")).foldLeft(Map[String, Expression]())((map, file) => map ++ loadFile(file.path))) match{
      case Some(map) => map
      case None => path.ifFile((f) => Parser.parseAll(Parser.program, f.bufferedReader) match{
        case Parser.Success(result, n) => result.statements.foldLeft(Map[String, Expression]())((map, statement) => statement match{
          case Assignment(name, List(), body) => map + (name -> body)
          case Assignment(name, args, body) => map + (name -> Lambda(args, body))
        })
        case Parser.Failure(msg, n) => {println(format("parse error in %s: %s", f.toString, msg)); Map[String, Expression]()}
        case Parser.Error(msg, n) => {println(msg); Map[String, Expression]()}
      }) match{
        case Some(map) => map
        case None => {println("invalid path: " + path); Map()}
      }
      
    }
  } 
    
}


 
