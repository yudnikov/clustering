package ru.yudnikov

import arithmetic._

object Arithmetic extends App {

  implicit val dictionary: Dictionary[Int] = List(0, 5, 10, 15, 20).toDictionary
  val a = List(0, 0, 5).toNumberLike
  val b = List(5).toNumberLike
  val c = a + b + b + b + b + b
  println(c)

  def combinations[T](target: NumberLike[T], source: NumberLike[T], f: NumberLike[T] => NumberLike[T], acc: List[NumberLike[T]] = Nil): List[NumberLike[T]] = {
    val current = f(source)
    if (current != target) {
      combinations(target, current, f, current :: acc)
    } else {
      current :: acc
    }
  }

  val target = List(20, 20, 20, 20, 20).toNumberLike
  val source = List(0, 0, 0, 0, 0).toNumberLike
  def f: NumberLike[Int] => NumberLike[Int] = x => x + List(5).toNumberLike
  val cs = combinations(target, source, f)

  cs foreach println

}
