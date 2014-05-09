package language

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.ImplicitConversions

object Parser extends JavaTokenParsers with ImplicitConversions{
  import Language._
  protected override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r //enable java-style comments
  
  lazy val identLower : Parser[String] = """[a-z][\w'\.]*""".r
  lazy val operator : Parser[String] = """[+-\*/]""".r
  lazy val identUpper : Parser[String] = """[A-Z_]\w*""".r
  
  val string : Parser[String] = "\"" ~> "[^\"]*".r <~ "\""
  val int : Parser[Int] = wholeNumber ^^ (_.toInt)  
  val hex : Parser[Int] = "0x" ~> """[0-f]*""".r ^?({
    case s => try {java.lang.Integer.parseInt(s, 16)} catch{case e:NumberFormatException => throw new MatchError}  
  }, {
    case s => format("unable to convert %s to integer", s)
  })
  lazy val program:Parser[Program] = statement.* ^^ Program
  
  lazy val statement:Parser[Statement] = 
    "type" ~> (identUpper <~ "=") ~ repsep(identUpper, "|") ^^ TypeBinding |
    "metric" ~> identUpper ~ identLower ~ repsep(identLower, " ") ~ ("=" ~> expression) ^^ (_ match{
    	case metricType ~ name ~ bindings ~ expr => MetricDefinition(metricType, name, Lambda(bindings, expr))
    }) |
    "import" ~> string ^^ Import |
    identLower ~ identLower.* ~ ("=" ~> expression) ^^ ValueBinding
    
    
  lazy val lambda:Parser[Lambda] =  ("\\" ~> identLower.*) ~ ("->" ~> expression) ^^ Lambda
    
  lazy val expression:Parser[Expression] = 
    //pattern matching
    "match" ~> expression ~ ("{" ~> (( ("_" ~> "->") ~> expression ^^ DefaultCase) | (identUpper ~ identLower.* ~ ("->" ~> expression) ^^ Case)).* <~ "}") ^^ Match |
    //SUGAR binary application
    "(" ~> expression ~ ("`" ~> identLower)  ~ ('`' ~> expression) <~ ")" ^^ (x => x match{case left ~ op ~ right => Application(Variable(op), List(left, right))}) |
    //SUGAR application
    ((identLower <~ "(") ~ repsep(expression, ",") <~ ")") ^^ (x => x match{case name ~ args => Application(Variable(name), args)}) |
    //SUGAR let binding
    "let" ~> (identLower ~ ("=" ~> expression)).* ~ ("in" ~> expression) ^^ (x => x match{case bindings ~ expr => Application(Lambda(bindings.map(_._1), expr), bindings.map(_._2))}) |
    //variable
    identLower ^^ Variable |
    //constructor
    identUpper ^^ ((x) => Constructor(x)) |
    //SUGAR List constructor
    "[" ~> repsep(expression, ",") <~ "]" ^^ (_.foldRight[Expression](Constructor("Nil"))((x, xs) => Application(Constructor("Cons"), List(x, xs)))) | 
    //integer
    (hex | int) ^^ Integer |
    //string
    string ^^ StringValue | 
    //lambda
    lambda |
    //application
    "(" ~> expression ~ expression.* <~ ")" ^^ Application
    
}