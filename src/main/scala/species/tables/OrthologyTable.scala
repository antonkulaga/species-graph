package species.tables

import better.files.File
import species.sparql._
import scala.collection.immutable._
import scala.compat._
import scala.collection.compat._
/**
 * Class that comparised orthology tables by concating
 */
class OrthologyTable(ensemblSpecies: IndexedSeq[EnsemblSpecies], g: Genes = Genes.default) extends GenesAggregator   {

  lazy val referenceSpecies: EnsemblSpecies = ensemblSpecies.head
  lazy val otherSpecies: IndexedSeq[EnsemblSpecies] = ensemblSpecies.tail
  lazy val speciesNames: IndexedSeq[String] = ensemblSpecies.map(_.latin_name)


  def writeOrthologyCounts(gs: Seq[String],
                           orthologyMode: OrthologyMode = OrthologyMode.default,
                           path: String = "/data/species/test_counts.tsv",
                           sl: Int = 1000, max_slides: Int = Int.MaxValue): File = {
    //val sl = 1000
    val geneSlides= gs.sliding(sl, sl).take(max_slides)
      .map(_.toVector).toVector
    //pprintln(species)
    val agg: Aggregation = orthologyMode.confidence.map(c=>agg_counts_with_confidence(c) _).getOrElse(agg_counts())
    writeAggregation(orthologyMode, path, geneSlides, agg)
    println("SUCCESSFULLY FINISHED!")
    File(path)
  }

  def writeOrthology(gs: Seq[String],
                     orthologyMode: OrthologyMode = OrthologyMode.default,
                     path: String = "/data/species/test.tsv",
                     sl: Int = 1000, max_slides: Int = Int.MaxValue
                    ): File = {
    val geneSlides: Vector[Vector[String]] = gs.sliding(sl, sl).take(max_slides)
      .map(_.toVector).toVector
    val agg: Aggregation = orthologyMode.confidence.map(c=>agg_concat_with_confidence(c) _).getOrElse(agg_concat_ids())
    writeAggregation(orthologyMode, path, geneSlides, agg)
    println(s"Finished writing orthology table for ${gs.length} genes by ${geneSlides.length} batches of ${sl}!")
    File(path)
  }

  protected def writeAggregation(orthologyMode: OrthologyMode,
                                 path: String,
                                 geneSlides: Vector[Vector[String]],
                                 agg: Map[String, Vector[Orthology]] => Map[String, String]): Unit = {
    for ((slide, i) <- geneSlides.zipWithIndex) {
      val orthologsBySpecies: Map[String, ListMap[String, Vector[Orthology]]] = g.get_orthologs(slide, orthologyMode).mapValues{ values =>
          ListMap.from(speciesNames.tail.map(s=>s->values.filter(o=>s.contains(o.species))))
      }
      val p = this.write_by_species(path, orthologsBySpecies , headers = (i == 0))(agg)
      println("slide_" + i + " :" + p.pathAsString)
    }
  }


  /**
   * Gets already aggregated results and write to the file
   * @param path to the file , if the file does not exist it will be created, otherwise - extended
   * @param genes genes found
   * @param headers if we need headers in the csv
   * @param sep separator (tab by default)
   * @param shorten_names if we want to get rid of redundant prefixes
   * @param agg_by_species
   * @return
   */
  def write_by_species(path: String,
                       genes:  Map[String, Map[String, Vector[Orthology]]],
                       headers: Boolean = true,
                       sep: String = "\t",
                       shorten_names: Boolean = true)(
                       agg_by_species: Aggregation
                      ): File = {
    val f = File(path).createFileIfNotExists(true)
    if(headers){
      val sp_headers = if(shorten_names)
        speciesNames.map(_.replace("http://aging-research.group/resource/", ""))
      else speciesNames
      f.appendLine(sp_headers.mkString(sep))
    }
    val grouped_genes: Map[String, Map[String, String]] = genes.mapValues(v=>agg_by_species(v))
    for{
      (reference, by_species) <- grouped_genes
      if by_species.nonEmpty
    } {
      val app = for(s <- speciesNames) yield by_species.getOrElse(s, "null")
      val ref = if(shorten_names) reference.replace("http://rdf.ebi.ac.uk/resource/ensembl/", "ens:") else reference
      f.appendLine(ref + sep + app.mkString(sep))
    }
    f
  }
}
