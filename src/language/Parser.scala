package language

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.ImplicitConversions

object Parser extends JavaTokenParsers with ImplicitConversions{
  
  protected override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r //enable java-style comments
  
  lazy val identLower : Parser[String] = """[a-z_]\w*""".r
  lazy val identUpper : Parser[String] = """[A-Z_]\w*""".r
  
  val string : Parser[String] = ("\""+"""([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*"""+"\"").r
  val int : Parser[Int] = wholeNumber ^^ (_.toInt)  
  lazy val program:Parser[Program] = statement.* ^^ Program
  
  lazy val statement:Parser[Statement] = 
    identLower ~ identLower.* ~ ("=" ~> expression) ^^ Assignment
    
  lazy val expression:Parser[Expression] = 
    "match" ~> expression ~ ("{" ~> (identUpper ~ identLower.* ~ ("->" ~> expression) ^^ Case).* <~ "}") ^^ Match |
    "(" ~> expression ~ ("`" ~> identLower)  ~ ('`' ~> expression) <~ ")" ^^ (x => x match{case left ~ op ~ right => Application(Variable(op), List(left, right))}) |
    identLower ^^ Variable |
    identUpper ^^ Constructor |
    "[" ~> repsep(expression, ",") <~ "]" ^^ (_.foldRight[Expression](Constructor("Empty"))((x, xs) => Application(Constructor("Cons"), List(x, xs)))) | 
    int ^^ Constant |
    ("\\" ~> identLower.*) ~ ("->" ~> expression) ^^ Lambda |
    "(" ~> expression ~ expression.+ <~ ")" ^^ Application
    
}