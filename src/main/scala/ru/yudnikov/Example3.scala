package ru.yudnikov

import org.joda.time.DateTime
import ru.yudnikov.clustering._

import scala.Function.tupled
import scala.util.Random

object Example3 extends App {

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
  val allValues = currentData.values.flatten.toList.distinct.sorted

  val initChromosome = Chromosome(List.fill(allValues.length)(0))

  val mutationRate: BigDecimal = 0.05
  val crossRate: BigDecimal = 0.9
  val elitePercent: BigDecimal = 0.1

  var population = {
    val individuals = (1 to 100).par map { i =>
      if (i == 1) {
        Individual(initChromosome, allValues)
      } else {
        Individual(initChromosome.mutated(mutationRate), allValues)
      }
    }
    Population(individuals.seq.toList, elitePercent)
  }

  var curGen = 0
  val maxGen = 1000
  while (!population.isSolved && curGen < maxGen) {
    population = population.crossed(crossRate, allValues).mutated(mutationRate, allValues)
    curGen += 1
    val lead = population.ordered.individuals.head
    println(s"fitness @$curGen is ${lead.fitness}")
    println(s"non covered: ${lead.clusterization.nonCovered.length}")
    println(s"clusters: ${lead.clusterization.clustersLength} from ${allValues.length}")
  }

  //population.ordered.individuals foreach println

}
