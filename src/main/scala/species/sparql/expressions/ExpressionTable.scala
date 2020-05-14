package species.sparql.expressions
import better.files.File
import species.sparql.orthology.{OrthologyManager, OrthologyMode}
import species.sparql.samples.SampleMini

import scala.collection.immutable._

case class ExpressionTable(referenceGenes: Vector[String], exp: MultiSpeciesExpressions) {

  lazy val isEmpty: Boolean = referenceGenes.isEmpty

  lazy val reference_size = referenceGenes.length

  def split_samples[K](fun: SampleMini => K): Map[K, ExpressionTable] = exp.samples.groupBy(fun).map{ case (s, samples)=>
    s-> ExpressionTable(referenceGenes, MultiSpeciesExpressions(samples.sortWith((a, b)=>a.lifespan > b.lifespan)))
  }


  def splitByTissue(): Map[String, ExpressionTable] = split_samples(s=>s.tissue)

  def splitByClass(): Map[String, ExpressionTable] = split_samples(s=>s.animal_class)
  def splitByClassTissue(): Map[(String, String), ExpressionTable] = split_samples(s=>s.animal_class->s.tissue)

  def skip(num: Int): ExpressionTable =  copy(referenceGenes = referenceGenes.drop(num))
  def take(num: Int): ExpressionTable = copy(referenceGenes = referenceGenes.take(num))
  def split(num: Int): (ExpressionTable, ExpressionTable) = (take(num), skip(num))


  def write_table(path: String,
                  mode: OrthologyMode,
                  header: Boolean = true,
                  sep: String = "\t",
                  withGeneNames : Boolean = false,
                  na: String = "N/A",
                  sl: Int = 10000, max_slides: Int = Int.MaxValue, rewrite: Boolean = false
                                   )(implicit orthologyManager: OrthologyManager) = {
    val f = File(path)
    if(rewrite){
        if(f.exists) {
          println("output file " + path + " exists, deleting it to write new output")
          f.delete()
        }

    }
    f.createFileIfNotExists(true)
    val sls = referenceGenes.sliding(sl, sl).toVector.zipWithIndex
    println(s"splitting ${referenceGenes.length} of reference genes into ${sls.length} batches of ${sl} genes")
    for((slide: Vector[String], i) <- sls)
    {
      val results: ExpressionResults = exp.expressionsResults(slide, mode)(orthologyManager)
      val rows = results.rows
      if(header && i==0) f.appendLine(rows.head.make_tsv_header(sep, withGeneNames))
      for(row <- rows){
        f.appendLine(row.as_tsv_simple_string(sep, withGeneNames = withGeneNames))
      }
      println("writing parts ["+(i+1) + " from "+sls.length +s"] to ${f.pathAsString}")
    }
    println(s"SUCCESSFULLY FINISHED WRITING EXPRESSIONS TO ${f.pathAsString}")
    f
  }
}
