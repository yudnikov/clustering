package ru.yudnikov

import org.joda.time.DateTime

import scala.Function.tupled
import scala.annotation.tailrec
import scala.math.BigDecimal
import scala.util.Random

package object clustering {

  private val random = new Random(new DateTime().getMillis)

  object Fatum {
    def apply(): Boolean = {
      Fatum(50)
    }
    def apply(value: Int): Boolean = {
      assume(0 <= value && value <= 100)
      val r = random.nextInt(100)
      r < value
    }
    def apply(value: BigDecimal): Boolean = {
      assume(0 <= value && value <= 1)
      val r = random.nextInt(100)
      r < (value * 100)
    }
  }

  case class Chromosome(genes: Seq[Int]) {
    def mutated(mutationRate: BigDecimal): Chromosome = {
      val mutatedGenes = genes.par map { gene =>
        if (Fatum(mutationRate)) {
          random.nextInt(20)
        } else {
          gene
        }
      }
      Chromosome(mutatedGenes.seq)
    }
    def crossed(that: Chromosome): Chromosome = {
      val crossedGenes = this.genes.zip(that.genes).par map tupled { (x, y) =>
        if (Fatum()) x else y
      }
      Chromosome(crossedGenes.seq)
    }
  }

  case class Individual(chromosome: Chromosome, allValues: List[Int]) {
    val clusterization = Clusterization(allValues, chromosome.genes)
    def fitness: BigDecimal = clusterization.fitness
    override def toString: String = fitness.toString()
  }

  case class Population(individuals: List[Individual], elitePercent: BigDecimal) {
    private val ordering = new Ordering[BigDecimal] {
      override def compare(x: BigDecimal, y: BigDecimal): Int = y compare x
    }
    lazy val ordered: Population = copy(individuals = individuals.sortBy(_.fitness)(ordering))

    def fitness: BigDecimal = individuals.take(eliteSize).map(_.fitness).sum / eliteSize

    def isSolved: Boolean = individuals.exists(_.fitness >= 3.33)

    private lazy val eliteSize: Int = (individuals.length * elitePercent).intValue()

    def crossed(crossRate: BigDecimal, allValues: List[Int]): Population = {
      val crossedIndividuals = ordered.individuals.zipWithIndex.par map tupled { (ind, index) =>
        if (index > elitePercent && Fatum(crossRate)) {
          Individual(ind.chromosome crossed select().chromosome, allValues)
        } else {
          ind
        }
      }
      Population(crossedIndividuals.seq.toList, elitePercent)
    }

    def mutated(mutationRate: BigDecimal, allValues: List[Int]): Population = {
      val mutatedIndividuals = ordered.individuals.zipWithIndex.par map tupled { (ind, index) =>
        if (index > elitePercent) {
          Individual(ind.chromosome.mutated(mutationRate), allValues)
        } else {
          ind
        }
      }
      Population(mutatedIndividuals.seq.toList, elitePercent)
    }

    @tailrec
    final def select(
      individuals: List[Individual] = ordered.individuals,
      target: Int = random.nextInt((fitness * 100000).toInt),
      agg: BigDecimal = 0
    ): Individual = {
      individuals match {
        case h :: Nil =>
          h
        case list if agg >= target =>
          list.head
        case list =>
          select(list.tail, target, agg + (list.head.fitness * 100000))
      }
    }
  }

}
