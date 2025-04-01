package u03

import u02.AlgebraicDataTypes.Person
import u02.Optionals.Option

import scala.annotation.tailrec

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

    @tailrec
    def drop[A](l: List[A], n: Int): List[A] = l match
      case Cons(_, t) if n > 1 => drop(t , n - 1)
      case Cons(_, t) if n == 1 => t
      case Nil() => Nil()

    def append[A](left: List[A], right: List[A]): List[A] = left match
      case Cons(h, t) => Cons(h, append(t, right))
      case Nil() => right

    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match
      case Nil() => Nil()
      case Cons(h, t) => append(f(h), flatMap(t)(f))

    def mapInTermsOfFlatMap[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Nil() => Nil()
      case Cons(h, t) => flatMap(l)(v => Cons(mapper(v), Nil()))

    def filterInTermsOfFlatMap[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(h, t) if pred(h) => flatMap(l1)(v => Cons(h, Nil()))
      case Nil() => Nil()

    import u02.Optionals.Option.*
    def max(l: List[Int]): Option[Int] = l match
      case Cons(h, t) if t == Nil() => Some(h)
      case Cons(h, t) => max(filter(t)(_ > h))
      case Nil() => None()

  val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))
  println(List.sum(l)) // 60

  import List.*
  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
  println(drop(l, 1)) // Cons (20 , Cons (30 , Nil ()))
  println(drop(l, 2)) // Cons (30 , Nil ())
  println(drop(l, 5)) // Nil ()
  val tail = Cons(40, Nil())
  println(append(Nil(), tail)) //Cons (40 , Nil ())
  println(append(l, tail)) // Cons (10 , Cons (20 , Cons (30 , Cons (40 , Nil ()))))

  import u02.Modules.Person.*

  def getCourses(l: List[Person]): List[String] = l match
    case Cons(Person.Teacher(_, c), t) => Cons(c, getCourses(t))
    case Cons(_, t) => getCourses(t)
    case Nil() => Nil()

  def getCoursesFlatMap(l: List[Person]): List[String] = flatMap(l)(p => p match
    case Person.Teacher(_, c) => Cons(c, Nil())
    case _ => Nil()
  )

