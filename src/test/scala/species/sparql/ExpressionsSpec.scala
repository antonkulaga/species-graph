package species.sparql

import org.scalatest.wordspec._
import species.sparql.orthology.{Orthology, OrthologyManager, OrthologyMode}

import scala.collection.immutable._
import org.scalatest.wordspec._
import species.sparql.expressions.ExpressionsData.{AllBySpecies, ReferenceGenesInSpecies}
import species.sparql.expressions.{ExpressionResults, MultiSpeciesExpressions, OrthologExpression, SameSpeciesExpressions}
import species.sparql.samples.Samples

class ExpressionsSpec extends AnyWordSpec {
  protected def cut(str: String) = str.substring(Math.max(str.lastIndexOf("/"),str.lastIndexOf(":"))+1)

  "expressions" should   {
    val server = "http://10.40.3.21:7200/"
    val s = new Samples(server)

   // lazy val ground_genes = Set("ens:ENSMUSG00000020053","ens:ENSSSCG00000000857","ens:ENSBTAG00000011082")
    val mtor = "ens:ENSG00000198793"
    val igf1 = "ens:ENSG00000017427"

    val genes = Vector(mtor, igf1)


    "work well for single species" in {

      val igf_ground_expressions = Set(
        "sra:SRR1521445"->1.519175,
        "sra:SRR3715877"->8.168431,
        "sra:SRR5008362"->1.654553,
        "sra:SRR5120939"->4.239582,
        "sra:SRR5120940"->3.159683)

      val mtor_ground_expressions = Set(
        "sra:SRR1521445"->	23.208749,
        "sra:SRR3715877"->	26.812367,
        "sra:SRR5008362"->	6.182178,
        "sra:SRR5120939"->	18.994536,
        "sra:SRR5120940"->	15.108637
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
    val samples = s.samples_mini_by_runs(Vector(
      "sra:SRR1521445", //human
      "sra:SRR3715877", //human
      "sra:SRR5115668", //mouse
      "sra:SRR1287653", //panda
      "sra:SRR1287654" //panda
    ))
    val exp = new MultiSpeciesExpressions(samples)
    implicit val orthologyManager = new OrthologyManager(server)


    "have reference genes expressions done right" in {
      val ref_exp: Seq[OrthologExpression] = exp.reference_expressions(genes)
      assert(ref_exp.size == 2)
      assert(ref_exp.map(o=>exp.localname(o.orthology.reference_gene)).toSet == genes.map(exp.localname(_)).toSet)
      val igf1_exp = ref_exp.filter(o=>exp.localname(o.orthology.reference_gene)==exp.localname(igf1)).head
      val ground_igf_exp = Set(
        "sra:SRR1521445"->1.519175,
        "sra:SRR3715877"->8.168431,
      )
      assert(igf1_exp.samples.toSet === ground_igf_exp)
    }
    "having orthologs expressions done right" in {
      //val ortho = exp.orh
      val orthologs: Vector[Orthology] = orthologyManager.orthologs(genes, OrthologyMode.one2one, exp.species)
      val results: AllBySpecies = exp.expressions_from_orthologs_by_species_ref(orthologs)
      assert(results.keySet.size === 3)
    }
    "having one2one results done right" in {

      val result_one2one: ExpressionResults = exp.expressionsResults(genes, OrthologyMode.one2one)
      assert(result_one2one.data.keySet.size ===3)
      assert(result_one2one.data.contains(":Ailuropoda_melanoleuca"))
      val Some(panda: ReferenceGenesInSpecies) = result_one2one.data.get(":Ailuropoda_melanoleuca")
      assert(panda.keySet.size ==1)
      assert(exp.contains_or_contained(panda.keySet.head, exp.localname(mtor)))
      val Some((panda_mtor, panda_exp)) = panda.collectFirst{ case (s, value) if exp.contains_or_contained(s,panda.keySet.head) => (s, value) }
      assert(panda_exp.head.samples.toVector == Vector(
        "sra:SRR1287653" -> 14.191812,
        "sra:SRR1287654" -> 6.442644
      ))
      val rows = result_one2one.rows
      assert(rows.map(_.referenceGene) == genes)
      println("--PANDA_2nd_row_one_to_one_TSV--")
      assert(result_one2one.rows.head.referenceGene === genes.head)
      assert(result_one2one.rows.tail.head.referenceGene === genes.tail.head)
      val second = result_one2one.rows.tail.head
      println(second.make_tsv_header(withGeneNames = true))
      println(second.as_tsv_simple_string(withGeneNames = true))
    }


    "having all results done right" in {
      val result_all: ExpressionResults = exp.expressionsResults(genes, OrthologyMode.all)
      assert(result_all.data.contains(":Ailuropoda_melanoleuca"))
      val Some(panda: ReferenceGenesInSpecies) = result_all.data.get(":Ailuropoda_melanoleuca")
      assert(panda.keySet === Set(mtor,igf1))

      assert(panda(mtor).size == 1)
      assert(panda(igf1).size == 2)

      val rows = result_all.rows
      assert(rows.map(_.referenceGene) === genes)
      assert(result_all.rows.head.referenceGene === genes.head)
      assert(result_all.rows.tail.head.referenceGene === genes.tail.head)
      val second = result_all.rows.tail.head

      println("--PANDA_1st_row_one_to_many--")
      pprint.pprintln(second)
      println("--PANDA_1st_row_one_to_many_TSV--")
      println(second.make_tsv_header(withGeneNames = true))
      println(second.as_tsv_simple_string(withGeneNames = true))
      println(second.as_tsv_sum_string(withGeneNames = true))
    }

  }


}
