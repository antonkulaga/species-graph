package species.sparql.expressions

import species.sparql.orthology.{Orthology, OrthologyManager, OrthologyMode}

import scala.collection.immutable._
import scala.collection.compat._


/**
 * This class takes samples and orthologyManager and allows extracting expression values from them
 * @param samples samples
 *                NOTE: in processing the samples we give results by species
 */
case class MultiSpeciesExpressions(samples: Vector[SampleMini]) {

  lazy val samplesBySpecies: Map[String, Vector[SampleMini]] = samples.groupBy(_.species)

  lazy val species = samplesBySpecies.keys.toVector

  lazy val expBySpecies: Map[String, SameSpeciesExpressions] =
    samplesBySpecies.map { case (key, values) => key -> SameSpeciesExpressions(values) }

  def expressionsResults(referenceGenes: Vector[String], mode: OrthologyMode)
                        (implicit orthologyManager: OrthologyManager): ExpressionResults = {
    val orthologs: Vector[Orthology] = orthologyManager.orthologs(referenceGenes, mode, species)
    val data = expressions_from_orthologs_by_species_ref(orthologs)
    ExpressionResults(referenceGenes, samples, mode, data)
  }

  def expressions_from_orthologs_by_species_ref(orthologs: Vector[Orthology]): ExpressionsData.AllBySpecies =
  {
    val orthologsBySpecies = orthologs.groupBy(_.species)
    (for {
      (species, exp) <- expBySpecies
      ortho: Vector[Orthology] = orthologsBySpecies.getOrElse(species, Vector.empty[Orthology])
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