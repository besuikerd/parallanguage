package language

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.ImplicitConversions

object Parser extends JavaTokenParsers with ImplicitConversions{
  
  protected override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r //enable java-style comments
  
  lazy val identLower : Parser[String] = """[a-z_][\w']*""".r
  lazy val identUpper : Parser[String] = """[A-Z_]\w*""".r
  
  val string : Parser[String] = ("\""+"""([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*"""+"\"").r
  val int : Parser[Int] = wholeNumber ^^ (_.toInt)  
  val hex : Parser[Int] = "0x" ~> """[0-f]*""".r ^?({
    case s => try {java.lang.Integer.parseInt(s, 16)} catch{case e:NumberFormatException => throw new MatchError}  
  }, {
    case s => format("unable to convert %s to integer", s)
    
  })
  lazy val program:Parser[Program] = statement.* ^^ Program
  
  lazy val statement:Parser[Statement] = 
    identLower ~ identLower.* ~ ("=" ~> expression) ^^ Assignment
    
  lazy val expression:Parser[Expression] = 
    //pattern matching
    "match" ~> expression ~ ("{" ~> (( ("_" ~> "->") ~> expression ^^ DefaultCase) | (identUpper ~ identLower.* ~ ("->" ~> expression) ^^ Case)).* <~ "}") ^^ Match |
    //SUGAR binary application
    "(" ~> expression ~ ("`" ~> identLower)  ~ ('`' ~> expression) <~ ")" ^^ (x => x match{case left ~ op ~ right => Application(Variable(op), List(left, right))}) |
    //SUGAR application
    ((identLower <~ "(") ~ repsep(expression, ",") <~ ")") ^^ (x => x match{case name ~ args => Application(Variable(name), args)}) |
    //SUGAR let binding
    "let" ~> identLower ~ ("=" ~> expression) ~ ("in" ~> expression) ^^ (x => x match{case name ~ binding ~ expr => Application(Lambda(List(name), expr), List(binding))}) |
    //variable
    identLower ^^ Variable |
    //constructor
    identUpper ^^ Constructor |
    //SUGAR List constructor
    "[" ~> repsep(expression, ",") <~ "]" ^^ (_.foldRight[Expression](Constructor("Empty"))((x, xs) => Application(Constructor("Cons"), List(x, xs)))) | 
    //integer
    (hex | int) ^^ Integer |
    //lambda
    ("\\" ~> identLower.*) ~ ("->" ~> expression) ^^ Lambda |
    //application
    "(" ~> expression ~ expression.+ <~ ")" ^^ Application
    
}