package species.sparql.orthology

import org.eclipse.rdf4j.sparqlbuilder.core.query.{Queries, SelectQuery}
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri
import species.sparql.QueryBase

import scala.collection.immutable._
import scala.collection.compat._

case class GeneInfo(gene: String, symbol: String, species: String)

object OrthologyManager{
  def human(serverURL: String ="http://10.40.3.21:7200/") = OrthologyManager("Homo_sapiens", serverURL)
  lazy val default = human()
}
case class OrthologyManager(speciesName: String = "Homo_sapiens",
                            override val serverURL: String ="http://10.40.3.21:7200/")
  extends QueryBase {

  def orthologs_by_ref(referenceGenes: Seq[String],orthologyMode: OrthologyMode,
                                 species: Seq[String]): Map[String, Vector[Orthology]] = {
    val ortho: Vector[Orthology] = orthologs(referenceGenes, orthologyMode, species)
    ortho.groupBy(f=>f.reference_gene)
  }


  /**
   * Returnes orthologs grouped by reference gene
   * @param referenceGenes genes for which we search orthologies
   * @param orthologyMode
   * @param species (if empty - gets all the species)
   * @return
   */
  def orthologs(referenceGenes: Seq[String],
                    orthologyMode: OrthologyMode,
                    species: Seq[String] = Vector.empty): Vector[Orthology] = {
    val query = s"""$commonPrefixes
                   |
                   |SELECT * WHERE
                   |{
                   |    ${values("species", species.map(u))}
                   |    ${values("reference_gene", referenceGenes.map(ens))} #put reference genes selected by the user
                   |    ${orthologyMode.as_values}
                   |    ${orthologyMode.with_confidence}
                   |    GRAPH ?confidence {
                   |      ?reference_gene ?orthology ?ortholog .
                   |	  }
                   |    ?species :has_gene ?ortholog .
                   |    ?species rdf:type :Species .
                   |    OPTIONAL { ?ortholog rdfs:label ?ortholog_symbol } .
                   |}
      """.stripMargin
    select_query(query).map(mp=>
      Orthology(
        shorten(mp("reference_gene")),
        shorten(mp("orthology")),
        shorten(mp("ortholog")),
        mp.getOrElse("ortholog_symbol", ""),
        shorten(mp("species")),
        mp("confidence")
      )
    )
  }

  def genesInfo(genes: Vector[String]): Vector[GeneInfo] = {
    val query =
      s"""
         |$commonPrefixes
         |SELECT ?gene ?symbol ?species
         |WHERE {
         |${values("gene", genes.map(u))}
         |?species :has_gene ?gene .
         |OPTIONAL { ?gene rdfs:label ?symbol } .
         |}
         |""".stripMargin
    val results: Seq[ListMap[String, String]] = select_query(query)
    results.map(mp=>GeneInfo(shorten(mp("gene")), mp.getOrElse("symbol", ""), shorten(mp("species")))).toVector
  }

  def speciesGeneInfo(species: Seq[String] = Vector(":Homo_sapiens")): Vector[GeneInfo] = {
    val query =
      s"""
         |$commonPrefixes
         |SELECT ?gene ?symbol ?species
         |WHERE {
         |${values("species", species.map(u))}
         |?species :has_gene ?gene .
         |OPTIONAL { ?gene rdfs:label ?symbol } .
         |}
         |""".stripMargin
    val results: Seq[ListMap[String, String]] = select_query(query)
    results.map(mp=>GeneInfo(shorten(mp("gene")), mp.getOrElse("symbol", ""), shorten(mp("species")))).toVector
  }


  def speciesGenes(species: String = "http://aging-research.group/resource/Homo_sapiens"): Vector[String] = {
    val query =
      s"""
        |$commonPrefixes
        |SELECT ?gene
        |WHERE { ${u(species)} :has_gene ?gene . }
        |""".stripMargin
    val results: Seq[ListMap[String, String]] = select_query(query)
    results.map(mp=>shorten(mp("gene"))).toVector
  }



  def shutDown() = {
    repository.shutDown();
    repositoryManager.shutDown();
  }

}
