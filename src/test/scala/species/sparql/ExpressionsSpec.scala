package species.sparql

import org.scalatest.wordspec._
import species.sparql.orthology.{OrthologyManager, OrthologyMode}

import scala.collection.immutable._
import org.scalatest.wordspec._
import species.sparql.expressions.{SameSpeciesExpressions, Samples}

class ExpressionsSpec extends AnyWordSpec {
  protected def cut(str: String) = str.substring(Math.max(str.lastIndexOf("/"),str.lastIndexOf(":"))+1)

  "expressions" should   {
    val server = "http://10.40.3.21:7200/"
    val s = new Samples(server)

    lazy val ground_genes = Set("ens:ENSMUSG00000020053","ens:ENSSSCG00000000857","ens:ENSBTAG00000011082")

    "work well for single species" in {
      val gene = Vector("ens:ENSG00000017427")

      val ground_expressions = Set(
        "https://www.ncbi.nlm.nih.gov/sra/SRR1521445"->1.519175,
        "https://www.ncbi.nlm.nih.gov/sra/SRR3715877"->8.168431,
        "https://www.ncbi.nlm.nih.gov/sra/SRR5008362"->1.654553,
        "https://www.ncbi.nlm.nih.gov/sra/SRR5120939"->4.239582,
        "https://www.ncbi.nlm.nih.gov/sra/SRR5120940"->3.159683)

      val samples = s.samples_mini_by_runs(Vector("sra:SRR1521445",
        "sra:SRR3715877",
        "sra:SRR5008362",
        "sra:SRR5120939",
        "sra:SRR5120940"))


      val runs = samples.map(_.run)
      assert(samples.map(s => cut(s.species)).toSet === Set("Homo_sapiens"))
      val human = SameSpeciesExpressions(samples, server)

      val exps = human.expressions(gene)
      pprint.pprintln(exps)
      assert(exps.size === 5)
      assert(exps.map(e => e.sample->e.value).toSet == ground_expressions)
    }

  }
}
