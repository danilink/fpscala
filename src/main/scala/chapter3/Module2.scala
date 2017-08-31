package chapter3

/**
  * Created by Danilo on 14/8/17.
  */
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A] (header: A, tail: List[A]) extends List[A]

object List {
  def sum ( inst: List[Int]) : Int = inst match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def apply[A] (as: A*) : List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*) )

  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(_,t) => t
    }

  def setHead[A](l :List[A], h:A ) : List[A] = l match {
    case Nil => Nil
    case Cons(_,t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_,t) => drop(t, n-1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h,t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  def main(args: Array[String]) {
    var x = List(5,3,2,1)
    println(x)
    println(sum(x))
    println(tail(x))
    println(x)
    x = setHead(x, 10)
    println(x)
    x = dropWhile(x, (a :Int) => a > 3)
    println(x)
  }
}



