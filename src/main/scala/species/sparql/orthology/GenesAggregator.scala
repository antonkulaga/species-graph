package species.sparql.orthology

import scala.collection.immutable._

trait GenesAggregator {

  def agg_counts()
                (species: Map[String, Vector[Orthology]]): Map[String, String] = {
    species.map{case (k, v)=>k-> v.length.toString}
  }
  def agg_counts_with_confidence(confidence: String = "high")
                                (species: Map[String, Vector[Orthology]]): Map[String, String] = {
    species.map{case (k, v)=>k->v.count(c => c.confidence.contains(confidence)).toString}
  }

  def agg_concat_ids(shorten: Boolean = true, sep: String = ";")
                    (species: Map[String, Vector[Orthology]]): Map[String, String] = {
    species.map{case (k, v)=>k->
      v.map(o => if (shorten) o.ortholog.replace("http://rdf.ebi.ac.uk/resource/ensembl/", "ens:") else o.ortholog).mkString(sep)}
  }

  def agg_concat_with_confidence(confidence:String = "high", shorten: Boolean = true, sep: String = ";")
                                (species: Map[String, Vector[Orthology]]): Map[String, String] = {
    species.map{case (k, v)=>k->v.filter(_.confidence.contains(confidence))
        .map(o => if (shorten) o.ortholog.replace("http://rdf.ebi.ac.uk/resource/ensembl/", "ens:") else o.ortholog).mkString(sep)}
  }

  def agg_concat_names(high_confidence_only: Boolean = false, sep: String = ";")
                      (species: Map[String, Vector[Orthology]]): Map[String, String] = {
    species.map{case (k, v)=>k->
      (if(high_confidence_only) v.filter(_.confidence.contains("high")) else v)
        .map(_.ortholog_symbol).mkString(sep)}
  }
}