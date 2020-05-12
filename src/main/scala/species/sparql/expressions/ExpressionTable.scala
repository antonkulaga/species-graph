package species.sparql.expressions
import better.files.File
import species.sparql.orthology.{OrthologyManager, OrthologyMode}

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
                  sl: Int = 1000, max_slides: Int = Int.MaxValue,
                                   )(implicit orthologyManager: OrthologyManager) = {
    val f = File(path).createFileIfNotExists(true)
    val sls = referenceGenes.sliding(sl, sl)
    for((slide, i) <- sls.zipWithIndex)
    {
      val results: ExpressionResults = exp.expressionsResults(slide, mode)(orthologyManager)
      val rows = results.rows
      if(header && i==0) f.appendLine(rows.head.header(sep, withGeneNames))
      for(row <- rows){

        f.appendLine(row.as_tsv_simple_string(sep, withGeneNames = withGeneNames))
      }
      println("WROTE ["+i + " from "+sls.length)

    }
    println(s"SUCCESSFULLY FINISHED WRITING EXPRESSIONS TO ${f.pathAsString}")
    f
  }
}
