package species.sparql.expressions

import species.sparql.QueryBase
import species.sparql.orthology.Orthology

import scala.collection.immutable._
import scala.collection.mutable


/**
 * Expression Data (how we extract it from the database)
 */
object ExpressionsData {
  //type ExpressionsBySamples = ListMap[String, Double]
  //type ByReferenceInSpecies = Map[String, Vector[OrthologExpression]]
  type ReferenceGenesInSpecies = Map[String, Vector[OrthologExpression]]
  type AllBySpecies = Map[String,  ReferenceGenesInSpecies ]
}


case class ExpressionValue(sample: String, gene: String, value: Double)
object OrthologExpression {
  def from(orthology: Orthology, expressions: Seq[ExpressionValue]) =  OrthologExpression(orthology,
    ListMap.from(expressions.map(e=>e.sample->e.value)))

  def merge(head: OrthologExpression, exps: Vector[OrthologExpression]) = {
    head.copy(samples = ListMap.from(head.samples ++ exps.collect{ case o if o.orthology.ortholog==head.orthology.ortholog => o.samples}.flatten))
  }
}
case class OrthologExpression(orthology: Orthology, samples: ListMap[String, Double])
{
  def merge(exps: Vector[OrthologExpression]): OrthologExpression = {
    copy(samples = ListMap.from(this.samples.toVector ++ exps.collect{ case o if o.orthology.ortholog==this.orthology.ortholog => o.samples}.flatten))
  }
}
