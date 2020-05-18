package species.sparql.orthology

import better.files.File
import species.sparql.samples.EnsemblSpecies
import wvlet.log.LogSupport

import scala.collection.compat._
import scala.collection.immutable._
import scala.compat._

object OrthologyTable {
  type Aggregation = Map[String, Vector[Orthology]]=>Map[String, String]
}
/**
 * Class that comparised orthology tables by concating
 */
class OrthologyTable(ensemblSpecies: IndexedSeq[EnsemblSpecies],
                     orthologyManager: OrthologyManager = OrthologyManager.default) extends GenesAggregator  with LogSupport  {
  import OrthologyTable.Aggregation
  lazy val referenceSpecies: EnsemblSpecies = ensemblSpecies.head
  //lazy val otherSpecies: IndexedSeq[EnsemblSpecies] = ensemblSpecies.tail
  lazy val speciesNames: IndexedSeq[String] = ensemblSpecies.map(_.latin_name)


  def writeOrthologyCounts(gs: Seq[String],
                           orthologyMode: OrthologyMode = OrthologyMode.default,
                           path: String = "/data/species/test_counts.tsv",
                           sl: Int = 1000, max_slides: Int = Int.MaxValue, na: String = "N/A"): File = {
    //val sl = 1000
    val geneSlides= gs.sliding(sl, sl).take(max_slides)
      .map(_.toVector).toVector
    //pprintln(species)
    val agg: Aggregation = orthologyMode.confidence.map(c=>agg_counts_with_confidence(c) _).getOrElse(agg_counts())
    writeAggregatedOrthologs(orthologyMode, path, geneSlides, na)( agg)
    info("SUCCESSFULLY FINISHED ORTHOLOGY COUNTING")
    File(path)
  }

  def writeOrthologs(gs: Seq[String],
                     orthologyMode: OrthologyMode = OrthologyMode.default,
                     path: String = "/data/species/test.tsv",
                     sl: Int = 1000, max_slides: Int = Int.MaxValue, na: String = "N/A"
                    ): File = {
    val geneSlides: Vector[Vector[String]] = gs.sliding(sl, sl).take(max_slides)
      .map(_.toVector).toVector
    val agg: Aggregation = orthologyMode.confidence.map(c=>agg_concat_with_confidence(c) _).getOrElse(agg_concat_ids())
    writeAggregatedOrthologs(orthologyMode, path, geneSlides, na)( agg)
    println(s"Finished writing orthology table for ${gs.length} genes by ${geneSlides.length} batches of ${sl}!")
    File(path)
  }

  protected def writeAggregatedOrthologs(orthologyMode: OrthologyMode,
                                         path: String,
                                         geneSlides: Vector[Vector[String]],
                                         na: String = "N/A")(agg: Aggregation): Unit =
    for ((slide, i) <- geneSlides.zipWithIndex) {
      val byReference: Map[String, Vector[Orthology]] = orthologyManager.orthologs_by_ref(slide, orthologyMode, speciesNames)
      val result: ListMap[String, Map[String, String]] = ListMap.from(
        for {
          g <- slide
          if byReference.contains(g)
        } yield g -> agg(byReference(g).groupBy(o => o.species))
      )
      val p = this.write_by_species(path, result, headers = (i == 0), na = na)
      info("slide_" + i + " :" + p.pathAsString)
    }


  /**
   * Gets already aggregated results and write to the file
   * @param path to the file , if the file does not exist it will be created, otherwise - extended
   * @param headers if we need headers in the csv
   * @param sep separator (tab by default)
   * @return
   */
  def write_by_species(path: String,
                       byReference:  ListMap[String, Map[String, String]],
                       headers: Boolean = true,
                       sep: String = "\t", na: String = "N/A")(
                      ): File = {
    val f = File(path).createFileIfNotExists(true)
    if(headers){
      f.appendLine(speciesNames.mkString(sep))
    }
    for{
      (reference, by_species) <- byReference
      if by_species.nonEmpty
    } {
      val app = for(s <- speciesNames.tail) yield by_species.getOrElse(s, na)
      f.appendLine(reference + sep + app.mkString(sep))
    }
    f
  }
}
