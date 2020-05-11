package species.sparql.expressions


import species.sparql.orthology.{Orthology, OrthologyManager, OrthologyMode}

import scala.collection.immutable._
import scala.collection.compat._

/**
 * Row of expression results
 * @param referenceGene
 * @param samplesExpressions
 */
case class ExpressionRow(referenceGene: String, samplesExpressions: ListMap[SampleMini, Vector[OrthologExpression]])
{
  def header(sep: String = "\t", withGeneNames:Boolean = false) = {
    "reference_gene" + sep + samplesExpressions.keys.map(s=>if(withGeneNames) "ortholog"+sep + s.run else s.run).mkString(sep)
  }

  def as_tsv_string(sep: String = "\t")(aggregate: Vector[OrthologExpression] => String): String = {
    referenceGene + sep + samplesExpressions.map{ case (s,v) => aggregate(v)}.mkString(sep)
  }

  def as_tsv_simple_string(sep: String = "\t",
                           sep2: String = ";", withGeneNames:Boolean = false,
                           na: String = "N/A"): String = {
    as_tsv_string(sep){
      case values if values.isEmpty => if(withGeneNames) na + sep + na + sep else na + sep
      case values =>
        val names = if(withGeneNames) {
          val ns = values.map(_.orthology.ortholog).mkString(sep2)
          if(ns.isEmpty) na + sep else sep
        } else ""
        names + values.map(v=>v.orthology).mkString(sep2) + sep
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
      ExpressionRow(ref,ListMap.from(samples.map{ case sample=>
        val species = sample.species
        val expressions =  for{
          by_ref: ExpressionsData.ReferenceGenesInSpecies <- data.get(species)
          refs: Vector[OrthologExpression] <- by_ref.get(ref)
        } yield {
          refs.collect{case exp if exp.samples.contains(sample.run) => exp.copy(samples = exp.samples.filter(_._1==sample.run))}
        }
        sample -> expressions.getOrElse(Vector.empty)
      }))
    }
  }
}
