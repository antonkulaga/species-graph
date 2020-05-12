package species.sparql.expressions

import species.sparql.QueryBase
import species.sparql.orthology.Orthology
import scala.collection.compat._
import scala.collection.immutable._

object SameSpeciesExpressions {
  def apply(samples: Vector[SampleMini], serverURL: String = "http://10.40.3.21:7200") = {
    require(samples.nonEmpty, "should have at least one sample")
    new SameSpeciesExpressions(samples.head.species, samples,serverURL)
  }
}
class SameSpeciesExpressions(val species: String,
                             val samples: Vector[SampleMini],
                             val serverURL: String = "http://10.40.3.21:7200") extends QueryBase{
  require(samples.forall(s=>s.species == species), "all samples should be of the same species")

  lazy val runs = samples.map(_.run)
  /**
   * Gets orthology expressions per reference gene in a species
   * @param orthologs
   * @return
   */
  def ortholog_expressions_by_ref(orthologs: Vector[Orthology]): ExpressionsData.ReferenceGenesInSpecies = {
    ortholog_expressions(orthologs).groupBy(_.orthology.reference_gene)
  }

  def ortholog_expressions(orthologs: Vector[Orthology]): Vector[OrthologExpression] = {
    val genes = orthologs.map(_.ortholog)
    val by_gene: Map[String, Vector[ExpressionValue]] = expressionsByGenesUnordered(genes)//.map{ case (g, ee)=> (g, OrthologExpression())}
    orthologs.collect{case o => OrthologExpression.from(o,by_gene.getOrElse(o.ortholog, Vector.empty[ExpressionValue]))}
  }

  def expressionsByGenesUnordered(genes: Seq[String]): Map[String, Vector[ExpressionValue]] = expressions(genes).groupBy(_.gene)

  /**
   * note: here we assume that runs  for the same species
   *
   * @param genes in the species that we want to extract expressions from
   */
  def expressions(genes: Seq[String]): Vector[ExpressionValue] = {
    val query =
      s"""${commonPrefixes}
         |SELECT ?run ?gene ?tpm WHERE
         |{
         |    ${values("run", runs.map(u))}
         |    ${values("gene", genes.map(g=>ens(g)))}
         |    ?expression :expression_of ?gene .
         |    ?run ?expression ?tpm .
         |}
         |""".stripMargin
    select_query(query).map(mp =>
      ExpressionValue(
        mp("run"),
        mp("gene"),
        mp("tpm").toDouble)
    )
  }
}
