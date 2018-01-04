package ru.yudnikov

import ru.yudnikov.Factor.sum

import scala.util.Try

package object arithmetic {

  implicit class ListExt[T](value: List[T]) {
    def toNumberLike: NumberLike[T] = NumberLike(value)
    def toDictionary: Dictionary[T] = Dictionary(value)
  }

  case class Dictionary[T](figures: List[T])

  case class NumberLike[T](figures: List[T]) {
    def +(that: NumberLike[T])(implicit dictionary: Dictionary[T]): NumberLike[T] = {
      sum(this, that)
    }
  }

  def sum[T](a: NumberLike[T], b: NumberLike[T], acc: List[T] = Nil, pos: Int = 0, overflow: Boolean = false)(implicit dictionary: Dictionary[T]): NumberLike[T] = {
    val max = math.max(a.figures.length, b.figures.length)
    if (pos == max && overflow) {
      NumberLike(dictionary.figures(1) :: acc)
    } else if (pos == max) {
      NumberLike(acc)
    } else {
      val ca = Try(a.figures(a.figures.length - 1 - pos))
      val cb = Try(b.figures(b.figures.length - 1 - pos))
      val ia = ca.map(dictionary.figures.indexOf).getOrElse(0)
      val ib = cb.map(dictionary.figures.indexOf).getOrElse(0)
      val is = ia + ib + {
        if (overflow) 1 else 0
      }
      val ic = is % dictionary.figures.length
      val cc = dictionary.figures(ic)
      sum(a, b, cc :: acc, pos + 1, is >= dictionary.figures.length)
    }
  }

}
