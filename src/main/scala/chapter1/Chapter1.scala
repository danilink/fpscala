package chapter1

/**
  * Created by Danilo on 1/8/17.
  */
object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAll(n: String, p: Int, f : Int => Int ) = {
    val msg = "The %s of %d is %d"
    msg.format(n, p, f(p))
  }

  def fib(n: Int) : Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, curr: Int): Int = {
      if (n == 0) prev
      else loop(n - 1, curr, prev + curr)
    }
    loop(n, 0, 1)
  }

  def fac(n: Int) : Int = {
    @annotation.tailrec
    def go(x: Int, acc: Int) : Int = {
      if (x <= 1) acc
      else go(x - 1, x * acc)
    }
    go(n, 1)
  }

  def isSorted [A] (array: Array[A], gthan: (A, A) => Boolean) : Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean =
      if (n >= array.length-1) true
      else if (gthan(array(n), array(n+1))) false
      else go(n+1)

    go(0)
  }

  def main(args: Array[String]): Unit =
    println(formatAll("abs", -42, abs))
    println(formatAll("fib", 2, fib))
    //max int number 2147483647
    println(formatAll("fac", 20, fac))
}
