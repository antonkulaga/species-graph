package species.sparql.expressions


import species.sparql.orthology.{Orthology, OrthologyManager, OrthologyMode}
import species.sparql.samples.SampleMini

import scala.collection.immutable._
import scala.collection.compat._

/**
 * Row of expression results
 * @param referenceGene
 * @param samplesExpressions
 */
case class ExpressionRow(referenceGene: String,
                         samplesExpressions: ListMap[SampleMini, Vector[OrthologExpression]])
{


  def make_tsv_header(sep: String = "\t",withGeneNames:Boolean = false): String = {
    "reference_gene" + sep + samplesExpressions.keys.map(s=>if(withGeneNames) s.species + sep + s.run else s.run).mkString(sep)
  }

  def as_tsv_string(sep: String = "\t")(aggregate: Vector[OrthologExpression] => String): String = {
    referenceGene + sep + samplesExpressions.map{ case (s,v) => aggregate(v)}.mkString(sep)
  }

  /**
   * Writes row as TSV
   * @param sep separator for TSV or CSV
   * @param sep2 separtor for several genes in a cell
   * @param withGeneNames if we should also add gene names (useful for debugging)
   * @param na how to mark N/A
   * @return string for the TSV file
   */
  def as_tsv_simple_string(sep: String = "\t",
                           sep2: String = ";",
                           withGeneNames:Boolean = false,
                           na: String = "N/A"): String = {
    as_tsv_string(sep){
      case cell if cell.isEmpty => if(withGeneNames) na + sep + na else na
      case cell =>
        val names = if(withGeneNames) {
          val ns = cell.map(_.orthology.ortholog).mkString(sep2)
          if(ns.nonEmpty) ns else na
        } else ""

        val vals: String = cell.map(v=>v.samples.headOption.map(_._2).getOrElse(na)).mkString(sep2)
        if(withGeneNames) names + sep + vals else vals
    }
  }

  def as_tsv_sum_string(sep: String = "\t",
                           sep2: String = ";",
                           withGeneNames:Boolean = false,
                           na: String = "N/A"): String = {
    as_tsv_string(sep){
      case cell if cell.isEmpty => if(withGeneNames) na + sep + na else na
      case cell =>
        val names = if(withGeneNames) {
          val ns = cell.map(_.orthology.ortholog).mkString(sep2)
          if(ns.nonEmpty) ns + sep else na + sep
        } else ""
        val vals= cell.map(v=>v.samples.headOption.map(_._2).getOrElse(0.0)).sum
        names + vals.toString
    }
  }

  def as_tsv_avg_string(sep: String = "\t",
                        sep2: String = ";",
                        withGeneNames:Boolean = false,
                        na: String = "N/A"): String = {
    as_tsv_string(sep){
      case values if values.isEmpty => if(withGeneNames) na + sep + na + sep else na + sep
      case values =>
        val names = if(withGeneNames) {
          val ns = values.map(_.orthology.ortholog).mkString(sep2)
          if(ns.nonEmpty) ns + sep else na + sep
        } else ""
        val vals= values.map(v=>v.samples.headOption.map(_._2).getOrElse(0.0)).sum / values.length.toDouble
        names + vals.toString
    }
  }
}
case class ExpressionResults(referenceGenes: Vector[String],
                             samples: Vector[SampleMini],
                             mode: OrthologyMode,
                             data: ExpressionsData.AllBySpecies) {

  lazy val isEmpty = referenceGenes.isEmpty || data.isEmpty

  lazy val rows: Vector[ExpressionRow] = {
    referenceGenes.map{ ref=>
      val samples_expressions: ListMap[SampleMini, Vector[OrthologExpression]] = ListMap.from(samples.map{ case sample=>
        val species = sample.species
        val expressions =  for{
          by_ref: ExpressionsData.ReferenceGenesInSpecies <- data.get(species)
          refs: Vector[OrthologExpression] <- by_ref.get(ref)
        } yield {
          refs.collect{case exp if exp.samples.contains(sample.run) => exp.copy(samples = exp.samples.filter(_._1==sample.run))}
        }
        sample -> expressions.getOrElse(Vector.empty)
      })
      ExpressionRow(ref, samples_expressions)
    }
  }
}
