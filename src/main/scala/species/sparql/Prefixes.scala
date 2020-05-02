package species.sparql

import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder
import org.eclipse.rdf4j.sparqlbuilder.rdf.{Iri, Rdf}

object Prefixes
class Prefixes {

  lazy val commonPrefixes: String =
    """
      |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      |PREFIX owl: <http://www.w3.org/2002/07/owl#>
      |PREFIX ens: <http://rdf.ebi.ac.uk/resource/ensembl/>
      |PREFIX samples:<http://aging-research.group/samples/>
      |PREFIX ncbi: <https://www.ncbi.nlm.nih.gov/>
      |PREFIX : <http://aging-research.group/resource/>
      |""".stripMargin


  lazy val prefixes: Map[String, String] = Map(
    "" -> "http://aging-research.group/resource/",
    "base" -> "http://aging-research.group/resource/",
    "ens" ->"http://rdf.ebi.ac.uk/resource/ensembl/",
    "samples" -> "http://aging-research.group/samples/"
  )

  def ens(str: String) = iri("ens:" + str)
  def u(str: String) = if(str.contains(":")) iri(str) else iri(":"+str)
  def samples(str: String) = iri("samples:" + str)

  def iri(str: String): Iri = str.indexOf(":") match {
    case -1 => Rdf.iri(str)
    case i =>
      val pre = str.substring(0, i)
      Rdf.iri(prefixes.get(pre).map(v => str.replace(pre + ":", v)).getOrElse(str))
  }
}
