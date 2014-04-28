package profiler

case class Profile(functions:Map[String, FunctionProfile])

case class FunctionProfile(args:List[Argument])

case class Argument(typ:Type, complexity:Expr)

case class Expr
case class Type

object Profiler {
	Profile(Map(
		    "name" -> FunctionProfile(List(
						Argument(Type(), Expr()),
						Argument(Type(), Expr())
		    		))
			
	))
	
}