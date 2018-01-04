package ru.yudnikov

import scala.annotation.tailrec
import scala.util.Try

object Factor extends App {

  @tailrec
  def factor(length: Int, n: Int, current: Int = 1, acc: BigInt = 1): BigInt = {
    if (current == n) {
      acc
    } else {
      factor(length - 1, n, current + 1, acc * length)
    }
  }

  val values = List(0, 5, 10, 15, 20)

  val m = 50
  val n = values.length

  println(factor(m, n))

  val root = List.fill(values.head)(m)

  @tailrec
  def sum(a: List[Int], b: List[Int], dictionary: List[Int], acc: List[Int] = Nil, pos: Int = 0, overflow: Boolean = false): List[Int] = {
    val max = math.max(a.length, b.length)
    if (pos == max && overflow) {
      dictionary(1) :: acc
    } else if (pos == max) {
      acc
    } else {
      val ca = Try(a(a.length - 1 - pos))
      val cb = Try(b(b.length - 1 - pos))
      val ia = ca.map(dictionary.indexOf).getOrElse(dictionary.head)
      val ib = cb.map(dictionary.indexOf).getOrElse(dictionary.head)
      val is = ia + ib + {
        if (overflow) 1 else 0
      }
      val ic = is % dictionary.length
      val cc = dictionary(ic)
      sum(a, b, dictionary, cc :: acc, pos + 1, is >= dictionary.length)
    }
  }

  val dict = List(0, 5, 10, 15, 20)
  val a = 0 :: 0 :: 0 :: Nil
  val b = 5 :: Nil
  val s = sum(sum(a, b, dict), b, dict)
  println(s)
}
