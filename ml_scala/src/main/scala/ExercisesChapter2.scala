/**
  * Created by jan on 12/21/2016.
  */
object ExercisesChapter2 {
  // 2.1
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(prev2: Int, prev1: Int, n: Int): Int = {
      if(n > 1) go(prev1, prev2+prev1, n-1)
      else prev2+prev1
    }
    if(n == 0) 0
    else go(1, 0, n)
  }

  // 2.2
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if(n >= as.length) true
      else if(!ordered(as(n-1), as(n))) false
      else loop(n+1)
    }
    if(as.length < 2) true
    else loop(1)
  }

  // 2.3
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a,b)

  // 2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  // 2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))
}
