package species.tables
import scala.collection.immutable._
import scala.collection.compat._
import better.files.File
import species.sparql.{Expression, ExpressionResults, Expressions, Genes, Orthology, OrthologyMode, SampleMini}

import scala.collection.immutable._
import species.sparql.ExpressionResults.ExpressionsBySamples
object ExpressionTable{

  def apply(g: Genes, exp: Expressions, samples: Vector[SampleMini], reference_species: String = "Homo_sapiens"): ExpressionTable ={
    val referenceGenes = g.speciesGenes(reference_species)
    ExpressionTable(referenceGenes, samples,  exp)
  }
}

case class ExpressionTable(referenceGenes: Vector[String], samples: Vector[SampleMini],  exp: Expressions) {

  /*
  def getExpressionsWithSum(genes: Vector[String], mode: OrthologyMode) = {
    val species = samplesBySpecies.keys.toVector
    val genes_by_species: Map[String, Map[String, Vector[Orthology]]] = g.get_orthologs_by_species(genes, mode, species)
    exp.get_expressions_in_samples(genes_by_species, samplesBySpecies)
  }

  def getExpressionsWithAgg(genes: Vector[String], mode: OrthologyMode): Map[String, Map[String, Vector[Expression]]] = {
    val species = samplesBySpecies.keys.toVector
    val genes_by_species: Map[String, Map[String, Vector[Orthology]]] = g.get_orthologs_by_species(genes, mode, species)
    exp.get_expressions_in_samples(genes_by_species, samplesBySpecies)
  }
   */


    /*
  def write_expression_table(path: String,
                                    expressions: ExpressionResults,
                                    header: Boolean = true,
                                    sep: String = "\t", withGeneNames : Boolean = false) = {
    val f = File(path).createFileIfNotExists(true)
    if(header)
      f.appendLine(expressions.makeHeader(sep, withGeneNames)+sep)
    val reference = expressions.expressions.head._2
    val reference_genes: IndexedSeq[String] = reference.map(_._1) //rows are based on the genes of reference species
    val size = reference.size
    for(i: Int <- (0 until size)) {
      f.append(reference_genes(i) + sep)
      for{
        (species: String, exp: IndexedSeq[(String, ExpressionsBySamples)]) <- expressions.expressions
        (gene, exps) = exp(i)
      }
      {
        val exp: Vector[String] = (if(withGeneNames) exps.values.map(v=>gene+sep+v).toVector else exps.values.map(v=>v.toString)).toVector
        val expStr = if(exp.isEmpty)
          ( if(withGeneNames) gene +sep + "null"+sep else "null"+sep) *
            expressions.samplesBySpecies("http://aging-research.group/resource/"+species).length
        else
          exp.mkString(sep) + sep
        f.append(expStr)
      }
      f.appendLine("")
    }
  }
     */

  def write_simple_expression_table(path: String,
                             expressions: ExpressionResults,
                             header: Boolean = true,
                             sep: String = "\t", withGeneNames : Boolean = false) = {
    val f = File(path).createFileIfNotExists(true)
    if(header)
      f.appendLine(expressions.makeHeader(sep, withGeneNames)+sep)
    val reference = expressions.expressions.head._2
    val reference_genes: IndexedSeq[String] = reference.map(_._1) //rows are based on the genes of reference species
    val size = reference.size
    for(i: Int <- (0 until size)) {
      f.append(reference_genes(i) + sep)
        for{
          (species: String, exp: IndexedSeq[(String, ExpressionsBySamples)]) <- expressions.expressions
          (gene, exps) = exp(i)
        }
        {
          val exp: Vector[String] = (if(withGeneNames) exps.values.map(v=>gene+sep+v).toVector else exps.values.map(v=>v.toString)).toVector
          val expStr = if(exp.isEmpty)
            ( if(withGeneNames) gene +sep + "null"+sep else "null"+sep) *
              expressions.samplesBySpecies("http://aging-research.group/resource/"+species).length
          else
            exp.mkString(sep) + sep
          f.append(expStr)
        }
        f.appendLine("")
    }
  }
}
