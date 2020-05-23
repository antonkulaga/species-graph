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


  def writeOrthologyCounts(gs: Seq[GeneInfo],
                           orthologyMode: OrthologyMode = OrthologyMode.default,
                           path: String = "/data/species/test_counts.tsv",
                           sl: Int = 1000, max_slides: Int = Int.MaxValue, na: String = "N/A", no_prefix: Boolean = true, empty_rows: Boolean = false): File = {
    //val sl = 1000
    val geneSlides= gs.sliding(sl, sl).take(max_slides)
      .map(_.toVector).toVector
    //pprintln(species)
    val agg: Aggregation = orthologyMode.confidence.map(c=>agg_counts_with_confidence(c) _).getOrElse(agg_counts())
    writeAggregatedOrthologs(orthologyMode, path, geneSlides, na, no_prefix = no_prefix, empty_rows = empty_rows)( agg)
    info("SUCCESSFULLY FINISHED ORTHOLOGY COUNTING")
    File(path)
  }

  def writeOrthologs(referenceGenes: Seq[GeneInfo],
                     orthologyMode: OrthologyMode = OrthologyMode.default,
                     path: String = "/data/species/test.tsv",
                     sl: Int = 1000, max_slides: Int = Int.MaxValue, na: String = "N/A", no_prefix: Boolean = true, empty_rows: Boolean
                    ): File = {
    val geneSlides = referenceGenes.sliding(sl, sl).take(max_slides)
      .map(_.toVector).toVector
    val agg: Aggregation = orthologyMode.confidence.map(c=>agg_concat_with_confidence(c) _).getOrElse(agg_concat_ids())
    writeAggregatedOrthologs(orthologyMode, path, geneSlides, na, no_prefix, empty_rows)( agg)
    println(s"Finished writing orthology table for ${referenceGenes.length} genes by ${geneSlides.length} batches of ${sl}!")
    File(path)
  }

  protected def writeAggregatedOrthologs(orthologyMode: OrthologyMode,
                                         path: String,
                                         geneSlides: Vector[Vector[GeneInfo]],
                                         na: String = "N/A", no_prefix: Boolean, empty_rows: Boolean)(agg: Aggregation): Unit = {
    val na_land = speciesNames.map(s=>s->na).toMap
    for ((slide, i) <- geneSlides.zipWithIndex) {
      require(geneSlides.nonEmpty && geneSlides.head.nonEmpty, "there should be genes to add orthology!")
      val sp = geneSlides.head.head.species //ugly fix
      require(geneSlides.forall(_.forall(g=>g.species == sp)), "by now only reference genes from the same species are allowed")
      val byReference: Map[String, Vector[Orthology]] = {
        orthologyManager.orthologs_by_ref(slide.map(_.gene), orthologyMode, speciesNames.filter(s=>s!=sp))
      }

      val result: ListMap[String, Map[String, String]] = ListMap.from(
        for {
          g <- slide
          in_results = byReference.contains(g.gene)
          if in_results || empty_rows
        } yield {
          val values: Map[String, String] = if(in_results) agg(byReference(g.gene).groupBy(o => o.species)) else na_land
          g.gene -> values
        }
      )
      val p = this.write_by_species(path, sp, result, headers = (i == 0), na = na, no_prefix = no_prefix, empty_rows = empty_rows)
      info("slide_" + i + " :" + p.pathAsString)
    }
  }


  /**
   * Gets already aggregated results and write to the file
   * @param path to the file , if the file does not exist it will be created, otherwise - extended
   * @param headers if we need headers in the csv
   * @param sep separator (tab by default)
   * @return
   */
  def write_by_species(path: String,
                       referenceSpecies: String,
                       byReference:  ListMap[String, Map[String, String]],
                       headers: Boolean = true,
                       sep: String = "\t", na: String = "N/A",
                       no_prefix: Boolean = true, empty_rows: Boolean = false)(
                      ): File = {
    def up(str: String) = if(no_prefix) orthologyManager.localname(str) else str

    val f = File(path).createFileIfNotExists(true)

    if(headers){
      f.appendLine(up(referenceSpecies)+sep+speciesNames.collect{ case s if s!=referenceSpecies=> up(s)}.mkString(sep))
    }
    for{
      (reference, by_species) <- byReference
      if by_species.nonEmpty
    } {
      val app = for(s <- speciesNames.filter(_!=referenceSpecies)) yield by_species.getOrElse(s, na).replace("ens:", "")
      f.appendLine(up(reference) + sep + app.mkString(sep))
    }
    f
  }
}
