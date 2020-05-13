package species.sparql.expressions

import species.sparql.QueryBase
import species.sparql.expressions.ExpressionsData.AllBySpecies
import species.sparql.orthology.{Orthology, OrthologyManager, OrthologyMode}

import scala.collection.immutable._
import scala.collection.compat._


/**
 * This class takes samples and orthologyManager and allows extracting expression values from them
 * @param samples samples
 *                NOTE: in processing the samples we give results by species
 */
case class MultiSpeciesExpressions(samples: Vector[SampleMini], serverURL: String = "http://10.40.3.21:7200") extends SpeciesExpressions {

  lazy val samplesBySpecies: Map[String, Vector[SampleMini]] = samples.groupBy(_.species)

  lazy val species = samplesBySpecies.keys.toVector

  lazy val expBySpecies: Map[String, SameSpeciesExpressions] =
    samplesBySpecies.map { case (key, values) => key -> SameSpeciesExpressions(values, serverURL) }

  /**
   * Function to get reference genes expressions
   * @param referenceGenes
   * @return
   */
  def reference_expressions(referenceGenes: Seq[String]): Vector[OrthologExpression] = {
    val query =
      s"""${commonPrefixes}
         |SELECT ?gene ?species ?run ?symbol ?tpm WHERE
         |{
         |    ${runs_str}
         |    ${values("gene", referenceGenes.map(g=>ens(g)))}
         |    ?species :has_gene ?gene .
         |    ?species rdf:type :Species .
         |    ?run samples:has_organism ?species .
         |    ?expression :expression_of ?gene .
         |    ?run ?expression ?tpm .
         |    OPTIONAL { ?gene rdfs:label ?symbol } .
         |}
         |""".stripMargin
    val queryResult = select_query(query).map(mp =>
      OrthologExpression(
        Orthology.self_orthology(shorten(mp("gene")),
          mp.getOrElse("symbol", ""),
          shorten(mp("species"))),
        ListMap(shorten(mp("run"))->mp("tpm").toDouble)
      )
    )
    queryResult.groupBy(_.orthology.reference_gene).map{ case (gene, o)=> if(o.size > 1) o.head.merge(o.tail) else o.head }.toVector
  }

  def reference_expressions_per_species(referenceGenes: Seq[String]): ExpressionsData.ReferenceGenesInSpecies  = {
    reference_expressions(referenceGenes).groupBy(_.orthology.reference_gene)
  }

  /**
   * Gives expression results object
   * @param referenceGenes reference genes
   * @param mode orthology mode
   * @param orthologyManager
   * @return Expression results with reference genes, samples metadata, orthology mode and expression results
   */
  def expressionsResults(referenceGenes: Vector[String], mode: OrthologyMode)
                        (implicit orthologyManager: OrthologyManager): ExpressionResults = {

    val orthologs: Vector[Orthology] = orthologyManager.orthologs(referenceGenes, mode, species)
    val ref =  reference_expressions_per_species(referenceGenes)
    val data_other: AllBySpecies = expressions_from_orthologs_by_species_ref(orthologs)
    val data = if(ref.nonEmpty) data_other.updated(ref.head._2.head.orthology.species,ref) else data_other
    ExpressionResults(referenceGenes, samples, mode, data)
  }

  def expressions_from_orthologs_by_species_ref(orthologs: Vector[Orthology]): ExpressionsData.AllBySpecies =
  {
    val orthologsBySpecies = orthologs.groupBy(_.species)
    (for {
      (species, exp) <- expBySpecies
      ortho: Vector[Orthology] = orthologsBySpecies.getOrElse(species, orthologsBySpecies.getOrElse(u(species), Vector.empty[Orthology]))
        } yield {
      species -> exp.ortholog_expressions_by_ref(ortho)
    }
    ).toMap
  }

  /*
  def makeHeader(sep: String = "\t", withSpecies: Boolean): String = "gene" + sep +
    (if(withSpecies) samples
      .map(s=>
        s.species.replace("http://aging-research.group/resource/", ":")+"_gene" + sep +
          s.run.replace("https://www.ncbi.nlm.nih.gov/sra/", "sra:")).mkString(sep)
    else samples.map(_.run.replace("https://www.ncbi.nlm.nih.gov/sra/", "sra:")).mkString(sep))
  */
}