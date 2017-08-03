package fpinscala.datastructures

import fpinscala.gettingstarted.MyModule.{fib, formatAbs}

object Runner {

  def exRunner(i:Int, l:List[Int], f: (List[Int]) => Unit): Unit = {
    println("Exercise " + i + ":")
    println("List: " + l)
    f(l)
    println("")
  }

  def ex2(l:List[Int]): Unit = {
    println("tail:" + List.tail(l))
  }
  def ex3(l:List[Int]): Unit = {
    println("setHead: " + List.setHead(l, 42))
  }
  def ex4(l:List[Int]): Unit = {
    println("drop: " + List.drop(l, 3))
  }
  def ex5(l:List[Int]): Unit = {
    println("drop while: " + List.dropWhile(l)((a) => a != 3))
  }
  def ex6(l:List[Int]): Unit = {
    println("My version of init: " + List.myinit(l))
    println("Their version of init: " + List.theirinit(l))
  }
  def ex8(l:List[Int]): Unit = {
    println("testing foldRight: " + List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))
  }
  def ex9(l:List[Int]): Unit = {
    println("length: " + List.length(l))
  }
  def ex10(l:List[Int]): Unit = {
    println("foldLeft:" + List.foldLeft(l, 0)(_ + _))
  }

  def compareFoldLeftAndFoldRight(l:List[Int]): Unit = {
    val summing = (a:Int, b:Int) => a + b
    try{
      println("Sum via foldRight:")
      println(List.foldRight(l, 0)(summing))
    }
    catch {
      case _: StackOverflowError => println("StackOverflowError")
    }
    try{
      println("Sum via foldLeft:")
      println(List.foldLeft(l, 0)(summing))
    }
    catch {
      case _: StackOverflowError => println("StackOverflowError")
    }
  }

  def main(args: Array[String]): Unit = {
    val list = List.range(1, 5)
    exRunner(2, list, ex2)
    exRunner(3, list, ex3)
    exRunner(4, list, ex4)
    exRunner(5, list, ex5)
    exRunner(6, list, ex6)
    exRunner(8, list, ex8)
    exRunner(9, list, ex9)
    exRunner(10, list, ex10)

    compareFoldLeftAndFoldRight(List.range(1,10000))
  }
}

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def range(a:Int, b: Int): List[Int] = {
    @annotation.tailrec
    def go(a:Int, b: Int, x:List[Int]):List[Int] = {
      if (a <= b) go(a+1, b, Cons(a, x))
      else x
    }
    go(a,b,Nil)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) =>  x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, tc) => Cons(h, tc)
  }

  def drop[A](l: List[A], n: Int): List[A] = (l,n) match {
    case (Nil, _) => Nil
    case (Cons(h, t), 0) => Cons(h, t)
    case (Cons(_, t), n) => drop(t, n-1)
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h,t) => {
      if (f(h)) dropWhile(t)(f)
      else Cons(h,t)
    }
  }

  def myinit[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def go(l: List[A], o:List[A]): List[A] = l match {
      case Nil => o
      case Cons(h, Cons(h2, Nil)) => go(Nil, Cons(h, o))
      case Cons(h,t) => go(t, Cons(h, o))
    }

    @annotation.tailrec
    def flip(l: List[A], o:List[A]): List[A] = l match {
      case Nil => o
      case Cons(h, t) => flip(t, Cons(h, o))
    }
    flip(go(l, Nil), Nil)
  }

  def theirinit[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, theirinit(t))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_,acc) => acc + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
        case Nil => z
        case Cons(h, t) => foldLeft(t, f(z,h))(f)
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = ???
}
