package species.sparql

import org.eclipse.rdf4j.sparqlbuilder.rdf.{Iri, Rdf}



class Prefixes {

  lazy val commonPrefixes: String =
    """
      |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      |PREFIX owl: <http://www.w3.org/2002/07/owl#>
      |PREFIX ens: <http://rdf.ebi.ac.uk/resource/ensembl/>
      |PREFIX samples:<http://aging-research.group/samples/>
      |PREFIX ncbi: <https://www.ncbi.nlm.nih.gov/>
      |PREFIX sra: <https://www.ncbi.nlm.nih.gov/sra/>
      |PREFIX : <http://aging-research.group/resource/>
      |""".stripMargin


  lazy val prefixes: Map[String, String] = Map(
    "" -> "http://aging-research.group/resource/",
    "base" -> "http://aging-research.group/resource/",
    "ens" ->"http://rdf.ebi.ac.uk/resource/ensembl/",
    "samples" -> "http://aging-research.group/samples/",
    "sra" -> "https://www.ncbi.nlm.nih.gov/sra/",
    "ncbi" -> "https://www.ncbi.nlm.nih.gov/"
  )


  def iri(str: String): Iri = str.indexOf(":") match {
    case -1 => Rdf.iri(str)
    case i =>
      val pre = str.substring(0, i)
      Rdf.iri(prefixes.get(pre).map(v => str.replace(pre + ":", v)).getOrElse(str))
  }

  def ens(str: String)= if(str.contains("ens:")) iriStr(str) else iriStr("ens:" + str)
  def u(str: String) = if(str.contains(":")) iriStr(str) else iriStr(":"+str)
  def samples(str: String) = if(str.contains("samples:")) iriStr(str)  else iriStr("samples:" + str)
  def sra(str: String) = if(str.contains("sra:")) iriStr(str)  else iriStr("sra:"+str)

  def iriStr(str: String) = str.indexOf(":") match {
    case -1 => if(str.startsWith("http")) "<" + str + ">" else str
    case i =>
      val pre = str.substring(0, i)
      val result = prefixes.get(pre).map(v => "<" + str.replace(pre + ":", v) + ">").getOrElse(str)
      if(result.startsWith("http")) "<" + result + ">" else result
  }
  //def unUri(str: String): String = if(str.startsWith("http")) "<" + str + ">" else str



  /**
   * Tries to extract localname (after prefix or "/")
   * @param str
   * @return
   */
  def localname(str: String)  = str.substring(Math.max(str.lastIndexOf("/"),str.lastIndexOf(":"))+1)
  def contains_or_contained(a: String, b: String): Boolean = a.contains(b) || b.contains(a)
  def contains_or_contained(seq: Seq[String], value: String): Boolean = seq.exists(contains_or_contained(_,value))

}
