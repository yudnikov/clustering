import org.joda.time.DateTime

import ru.yudnikov.clustering._

import scala.Function.tupled
import scala.annotation.tailrec
import scala.util.Random


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
          random.nextInt(1000) * 10
        }
      }
    }
  }.toMap
  segmentToTimes
}

val currentData = data(3, 50)

val allValues = currentData.values.flatten.toList.distinct.sorted

val genes = 1 to allValues.size map { _ =>
  if (Fatum(0.95)) 20 else 0
}

math.exp(0)

