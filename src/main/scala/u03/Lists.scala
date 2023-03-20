package u03

import u02.Optionals.Option

object Lists extends App :

  // A generic linkedlist
  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()
  // a companion object (i.e., module) for List
  object List:
    
    def sum(l: List[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()

    def drop[A](l: List[A], n: Int): List[A] = l match
      case Cons(_, t) if n > 1 => drop(t, n - 1)
      case Cons(_, t) if n == 1 => t
      case Nil() => Nil()
    def append[A](left: List[A], right: List[A]): List[A] = left match
      case Nil() => right;
      case Cons(h, t) => Cons(h, append(t, right))
    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = list match
      case Cons(h, t) => append(f(h), flatMap(t)(f))
      case Nil() => Nil();

    import u02.Optionals.Option.*
    def max(l: List[Int]): Option[Int] = l match
      case Cons(h, t) => max(t);
      case Nil() => None();


  val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))
  println(List.sum(l)) // 60

  import List.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
