package species.sparql
import scala.collection.compat._
import scala.collection.immutable._

case class Expression(sample: String, gene: String, value: Double)
import scala.collection.compat._

object ExpressionResults{
  type ExpressionsBySamples = ListMap[String, Double]
  type PerSpecies = IndexedSeq[(String, ExpressionsBySamples)]
  type AllExpressions = ListMap[String, PerSpecies]

}
case class ExpressionResults(samples: IndexedSeq[SampleMini],
                             expressions: ExpressionResults.AllExpressions) {
  lazy val samplesBySpecies: Map[String, IndexedSeq[SampleMini]] = samples.groupBy(_.species)
  //lazy val species: IndexedSeq[String] = samples.map(_.species).distinct

  def makeHeader(sep: String = "\t", withSpecies: Boolean): String = "gene" + sep +
    (if(withSpecies) samples
      .map(s=>
        s.species.replace("http://aging-research.group/resource/", ":")+"_gene" + sep +
          s.run.replace("https://www.ncbi.nlm.nih.gov/sra/", "sra:")).mkString(sep)
    else samples.map(_.run.replace("https://www.ncbi.nlm.nih.gov/sra/", "sra:")).mkString(sep))
}

object Expressions extends Expressions
class Expressions  extends QueryBase {

  //def get_expressions(genes: Seq[String], samples: Seq[SampleMini]): Vector[Expression] = get_expressions(genes, samples.map(_.run))

  /**
   * Returns expressions
   * @param genes_by_species
   * @param samplesBySpecies
   * @return
   */
  def get_expressions_in_samples(genes_by_species: Map[String, Map[String, Vector[Orthology]]], samplesBySpecies: Map[String, Vector[String]]) = {
    for{
      (ref: String, bySpecies: Map[String, Vector[Orthology]]) <- genes_by_species
      (species, ortho) <- bySpecies
      samples = samplesBySpecies(species)
    } yield {
      val gs = ortho.map(_.ortholog)
      ref -> get_expressions_same_species(gs, samples).groupBy(_.sample)
    }
  }

  def get_expressions_in_samples(
                                 species2genes: Seq[(String, Seq[String])],
                                 samples: IndexedSeq[SampleMini]
                                ): ExpressionResults = {

    var ugly_runs = Vector.empty[SampleMini] //ugly workaround to accumulate runs information
    //val species2samples = samples.groupBy(s=>s.species)
    val seq = for {
      (sp, gs) <- species2genes
      runs = samples.filter(s => s.species.contains(sp))
      if runs.nonEmpty
    } yield {
      ugly_runs = ugly_runs ++ runs
      //println("SPECIES = " + sp)
      sp -> {
        val mp: Map[String, ListMap[String, Double]] = get_expressions_same_species(gs, runs.map(_.run))
          .groupBy(_.gene)
          .mapValues(ee => ListMap.from(ee.map(e => e.sample -> e.value)))
        val samplesExpression =
          (for (g <- gs) yield g -> mp.getOrElse(g, ListMap.empty[String, Double])).toVector
        samplesExpression
      }
    }
    ExpressionResults(ugly_runs, ListMap.from(seq))
  }

  /**
   * note: here we assume that runs  for the same species
   *
   * @param genes in the species that we want to extract expressions from
   * @param samples
   */
  def get_expressions_same_species(genes: Seq[String], samples: Seq[String]): Vector[Expression] = {
    val samplesStr = samples.map { s => unUri(s) }.mkString(" ")
    val geneStr = genes.map(g => "samples:has_" + g.replace("ens:", "").replace("http://rdf.ebi.ac.uk/resource/ensembl/", "") + "_expression").mkString(" ")
    val query =
      s"""${commonPrefixes}
         |
         |PREFIX sra: <https://www.ncbi.nlm.nih.gov/sra/>
         |SELECT * WHERE
         |{
         |    values ?run { $samplesStr  } . #put runs that the user selected
         |    values ?expression { $geneStr } . #put selected genes in a way samples:has_<ensemble_gene_id>_expression
         |    ?run ?expression ?tpm .
         |}
         |""".stripMargin
    get_query(query).map(mp =>
      Expression(
        mp("run"),
        mp("expression").replace("http://aging-research.group/samples/has_", "ens:").replace("_expression", ""),
        mp("tpm").toDouble)
    )
  }
}