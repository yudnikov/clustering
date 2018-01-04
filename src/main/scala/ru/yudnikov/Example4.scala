package ru.yudnikov

object Example4 extends App {

  /*
  val random = new Random(new DateTime().getMillis)

  def data(m: Int, n: Int): Map[Int, Seq[Int]] = {
    val segmentToTimes = {
      for {
        i <- 1 to m
      } yield {
        i -> {
          for {
            j <- 1 to n
          } yield {
            random.nextInt(200) * 10
          }
        }
      }
    }.toMap
    segmentToTimes
  }

  val currentData = data(5, 30)
  */

  def solve(currentData: Map[Int, Seq[Int]]): List[Set[Iterable[Int]]] = {
    val allValues = currentData.values.flatten.toList.distinct.sorted
    val genes = List.fill(20)(allValues.length)

    val allClusters = allValues.map { value =>
      Clusterization.getNeighbours(value, allValues, 20)
    }.toSet

    val variants = allClusters map { cluster =>
      cluster :: {
        allClusters - cluster
      }.toList
    }

    val solutions = variants.map { variant =>
      Clusterization.filterDistinct(variant)
    }.toList.filter(_.flatten.forall(x => allValues.contains(x))).sortBy(_.length).map(_.toSet)

    val withLength = solutions.map { sln =>
      sln.flatten.size
    }

    solutions
  }



}
