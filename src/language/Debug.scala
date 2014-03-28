package language

object Debug {
	def trace[A](pre:String)(block: A):A = {
	  val result = block
	  println(pre + ": " + result)
	  result
	}
	
	def traceFormatted[A](form:String)(value: A)(args: (A) => List[Any]):A = {
	  println(format(form, args(value)))
	  value
	}
}