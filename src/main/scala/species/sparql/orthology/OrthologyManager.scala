package species.sparql.orthology

import org.eclipse.rdf4j.sparqlbuilder.core.query.{Queries, SelectQuery}
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri
import species.sparql.QueryBase

import scala.collection.immutable._
import scala.collection.compat._

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
    val orthology =
      s"""
         |  values ?orthology { ${if(orthologyMode.one2one) "ens:ortholog_one2one" else ""} ${if(orthologyMode.one2many) "ens:ortholog_one2many" else ""} ${if(orthologyMode.many2many) "ens:ortholog_many2many" else ""}  } .
         |""".stripMargin
    val conf = orthologyMode.confidence.fold("")(s=>s"values ?confidence { <http://rdf.ebi.ac.uk/resource/ensembl/confidence/${s}> }")
    val query = s"""$commonPrefixes
                   |
                   |SELECT * WHERE
                   |{
                   |    ${values("species", species.map(u))}
                   |    ${values("reference_gene", referenceGenes.map(ens))} #put reference genes selected by the user
                   |    ${conf}
                   |    GRAPH ?confidence {
                   |      ${orthology}
                   |      ?reference_gene ?orthology ?ortholog .
                   |	  }
                   |    ?species :has_gene ?ortholog .
                   |    ?species rdf:type :Species .
                   |    OPTIONAL { ?ortholog rdfs:label ?ortholog_symbol } .
                   |}
      """.stripMargin
    //println("=============")
    //println(query)
    //println("=============")
    select_query(query).map(f=>Orthology(f))
  }


  def speciesGenes(species: String = "http://aging-research.group/resource/Homo_sapiens"): Vector[String] = {
    import RDF._
      val gene = variable("gene")
    val sp = if(species.contains(":")) species else s"http://aging-research.group/resource/${species}"
      val selectQuery: SelectQuery = Queries.SELECT(gene)
      .where(triple(
        sp,
        "http://aging-research.group/resource/has_gene",
        "?gene")
    )
      val results =select_query(selectQuery.getQueryString)
      results.map(v=>v("gene"))
  }



  def shutDown() = {
    repository.shutDown();
    repositoryManager.shutDown();
  }

}
