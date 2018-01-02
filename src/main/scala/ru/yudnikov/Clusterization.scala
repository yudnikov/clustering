package ru.yudnikov

import scala.Function.tupled
import scala.annotation.tailrec

case class Clusterization(allValues: List[Int], genes: Seq[Int]) {

  private def getNeighbours(value: Int, values: Iterable[Int], percent: BigDecimal): Iterable[Int] = {
    val halfPercent = percent / 2
    val left = value * (100 - halfPercent) / 100
    val right = value * (100 + halfPercent) / 100
    values.filter(x => left < x && x < right || x == value)
  }

  @tailrec
  private def filterDistinct(values: List[Iterable[Int]], acc: List[Iterable[Int]] = Nil, collected: List[Int] = Nil): List[Iterable[Int]] = {
    values match {
      case Nil => acc
      case h :: _ =>
        val current = h.toList
        if (collected.intersect(current).isEmpty) {
          filterDistinct(values.tail, h :: acc, collected union current)
        } else {
          filterDistinct(values.tail, acc, collected)
        }
    }
  }

  val settled: List[Iterable[Int]] = {
    allValues.zipWithIndex map tupled { (value, index) =>
      getNeighbours(value, allValues, genes(index))
    }
  }
  val clusters: List[Iterable[Int]] = filterDistinct(settled)
  val clustersLength: Int = clusters.length
  val covered: List[Int] = clusters.flatten
  val nonCovered: List[Int] = allValues diff covered
  val isValid: Boolean = nonCovered.isEmpty
  val fitness: BigDecimal = {
    BigDecimal(allValues.length) / (clustersLength + math.exp(math.min(nonCovered.length, 5) * 10))
  }

}
