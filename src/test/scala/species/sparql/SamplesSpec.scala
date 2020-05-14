package species.sparql
import org.scalatest.wordspec._
import species.sparql.orthology.{OrthologyManager, OrthologyMode}

import scala.collection.immutable._
import org.scalatest.wordspec._
import species.sparql.expressions.{ExpressionResults, MultiSpeciesExpressions, SameSpeciesExpressions}
import species.sparql.samples.Samples

class SamplesSpec extends AnyWordSpec  {
  "samples extractor" should {
    "get proper samples metadata" in {
      val s = new Samples()
      val samples = s.samples_mini_by_runs(Vector(
        "sra:SRR1521445", //human
        "sra:SRR3715877", //human
        "sra:SRR5115668", //mouse
        "sra:SRR1287653" //panda
      ))
      assert(samples.size == 4)
      val human1 = samples.filter(_.run.contains("SRR1521445")).head
      assert(human1.species.contains("Homo_sapiens"))
      assert(human1.tissue.contains("Brain"))
      assert(human1.lifespan == 122.5)
      val panda = samples.filter(_.run.contains("SRR1287653")).head
      assert(panda.lifespan === 36.8)
    }
  }
}
