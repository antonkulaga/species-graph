package species.sparql

import org.scalatest.wordspec._
import species.sparql.orthology.{OrthologyManager, OrthologyMode}

import scala.collection.immutable._
import org.scalatest.wordspec._
import species.sparql.expressions.{ExpressionResults, MultiSpeciesExpressions, OrthologExpression, SameSpeciesExpressions, Samples}

class ExpressionsSpec extends AnyWordSpec {
  protected def cut(str: String) = str.substring(Math.max(str.lastIndexOf("/"),str.lastIndexOf(":"))+1)

  "expressions" should   {
    val server = "http://10.40.3.21:7200/"
    val s = new Samples(server)

   // lazy val ground_genes = Set("ens:ENSMUSG00000020053","ens:ENSSSCG00000000857","ens:ENSBTAG00000011082")
    val igf1 = "ens:ENSG00000017427"
    val mtor = "ens:ENSG00000198793"

    val genes = Vector(mtor, igf1)


    "work well for single species" in {

      val igf_ground_expressions = Set(
        "https://www.ncbi.nlm.nih.gov/sra/SRR1521445"->1.519175,
        "https://www.ncbi.nlm.nih.gov/sra/SRR3715877"->8.168431,
        "https://www.ncbi.nlm.nih.gov/sra/SRR5008362"->1.654553,
        "https://www.ncbi.nlm.nih.gov/sra/SRR5120939"->4.239582,
        "https://www.ncbi.nlm.nih.gov/sra/SRR5120940"->3.159683)

      val mtor_ground_expressions = Set(
        "https://www.ncbi.nlm.nih.gov/sra/SRR1521445"->	23.208749,
        "https://www.ncbi.nlm.nih.gov/sra/SRR3715877"->	26.812367,
        "https://www.ncbi.nlm.nih.gov/sra/SRR5008362"->	6.182178,
        "https://www.ncbi.nlm.nih.gov/sra/SRR5120939"->	18.994536,
        "https://www.ncbi.nlm.nih.gov/sra/SRR5120940"->	15.108637
      )

      val ground_expressions = igf_ground_expressions ++ mtor_ground_expressions

      val samples = s.samples_mini_by_runs(Vector("sra:SRR1521445",
        "sra:SRR3715877",
        "sra:SRR5008362",
        "sra:SRR5120939",
        "sra:SRR5120940"))


      val runs = samples.map(_.run)
      assert(samples.map(s => cut(s.species)).toSet === Set("Homo_sapiens"))
      val human = SameSpeciesExpressions(samples, server)

      val exps_igf1 = human.expressions(Seq(igf1))
      //pprint.pprintln(exps)
      assert(exps_igf1.size === 5)
      assert(exps_igf1.map(e => e.sample->e.value).toSet == igf_ground_expressions)

      val exps_genes = human.expressions(genes)

      assert(exps_genes.size === 10)
      assert(exps_genes.map(e => e.sample->e.value).toSet == ground_expressions)

    }

    "work well with miltiple species" in {

      val samples = s.samples_mini_by_runs(Vector(
        "sra:SRR1521445", //human
        "sra:SRR3715877", //human
        "sra:SRR5115668", //mouse
        "sra:SRR1287653" //panda
        ))
      val exp = new MultiSpeciesExpressions(samples)
      implicit val orthologyManager = new OrthologyManager(server)
      val ref_exp: Seq[OrthologExpression] = exp.reference_expressions(genes)
      pprint.pprintln(ref_exp)
      assert(ref_exp.size == 2)
      assert(ref_exp.map(o=>exp.localname(o.orthology.reference_gene)).toSet == genes.map(exp.localname(_)).toSet)
      println("===========")
      pprint.pprintln(ref_exp)
      val igf1_exp = ref_exp.filter(o=>exp.localname(o.orthology.reference_gene)==exp.localname(igf1)).head
      val ground_igf_exp = Set(
        "https://www.ncbi.nlm.nih.gov/sra/SRR1521445"->1.519175,
        "https://www.ncbi.nlm.nih.gov/sra/SRR3715877"->8.168431,
      )
      assert(igf1_exp.samples.toSet === ground_igf_exp)
      //val result_one2one: ExpressionResults = exp.expressionsResults(genes, OrthologyMode.one2one)
      //assert(result_one2one.referenceGenes == genes)
      //assert(result_one2one.rows.map(_.referenceGene) == genes)
      //assert(result_one2one.rows.map(_.samplesExpressions))
      println("?????????????????????")
      //pprint.pprintln(result_one2one)
      //assert(result_one2one.referenceGenes == genes)
    }
  }


}
