package utils;

/**
 * allows 'tapping into' an underlying expression
 */
class Tap[A](underlying:A){
  def tap[B](func:(A) => B) = func(underlying)
}

object Tap{
	implicit def any2Tap[A](underlying:A) = new Tap(underlying)
}

