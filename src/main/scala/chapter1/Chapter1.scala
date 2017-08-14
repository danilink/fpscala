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

  def binarySearch[A](as: Array[A], key: A, gt: (A,A) => Boolean): Int = {
    @annotation.tailrec
    def go(low: Int, mid: Int, high: Int): Int = {
      if (low > high) -mid - 1
      else {
        val mid2 = (low + high) / 2
        val a = as(mid2)
        val greater = gt(a, key)
        if (!greater && !gt(key,a)) mid2
        else if (greater) go(low, mid2, mid2-1)
        else go(mid2 + 1, mid2, high)
      }
    }
    go(0, 0, as.length - 1)
  }

  def main(args: Array[String]): Unit =
    println(formatAll("abs", -42, abs))
    println(formatAll("fib", 2, fib))
    //max int number 2147483647
    println(formatAll("fac", 10, fac))
    println(isSorted(Array(50, 9, 6), (a:Int, b:Int) => a < b))
}
