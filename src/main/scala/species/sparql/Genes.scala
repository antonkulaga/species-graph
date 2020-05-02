package species.sparql
import better.files._
import File._
import java.io.{File => JFile}

import org.eclipse.rdf4j.sparqlbuilder.core.query.{Queries, SelectQuery}
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri

import scala.collection.compat._
import scala.collection.immutable._

object Genes{
  def human(serverURL: String ="http://10.40.3.21:7200/") = Genes("Homo_sapiens", serverURL)
  lazy val default = human()
}
case class Genes(speciesName: String = "Homo_sapiens",
                 override val serverURL: String ="http://10.40.3.21:7200/")
  extends QueryBase {
  val referenceSpecies: Iri = u(speciesName)

  def get_orthologs(genes: Seq[String], orthologyMode: OrthologyMode): Map[String, Vector[Orthology]] = {
    val gs = genes.map(unUri)
    val orthology =
      s"""
         |  values ?orthology { ${if(orthologyMode.one2one) "ens:ortholog_one2one" else ""} ${if(orthologyMode.one2many) "ens:ortholog_one2many" else ""} ${if(orthologyMode.many2many) "ens:ortholog_many2many" else ""}  } .
         |""".stripMargin
    val query = s"""$commonPrefixes
                   |
                   |SELECT * WHERE
                   |{
                   |    values ?reference_gene { ${gs.mkString(" ")} } . #put reference genes selected by the user
                   |    GRAPH ?confidence {
                   |      ${orthology}
                   |      ?reference_gene ?orthology ?ortholog .
                   |	}
                   |    ?species :has_gene ?ortholog .
                   |    ?species :has_taxon ?any .
                   |    ?ortholog rdfs:label ?ortholog_symbol
                   |}
                   |ORDER BY ?orthology ?confidence
      """.stripMargin
    get_query(query).map(f=>Orthology(f)).groupBy(f=>f.reference_gene)
  }


  /**
   * Returns orthologs grouped by reference genes and then species
   * @param genes
   * @param orthologyMode
   * @param species
   * @return
   */
  def get_orthologs_by_species(genes: Seq[String], orthologyMode: OrthologyMode, species: Seq[String]): Map[String, Map[String, Vector[Orthology]]] = {
    get_orthologs(genes, orthologyMode, species: Seq[String])
      .mapValues(f=>f.groupBy(_.species))
  }


  /**
   * Returnes orthologs grouped by reference gene
   * @param referenceGenes
   * @param orthologyMode
   * @param species
   * @return
   */
  def get_orthologs(referenceGenes: Seq[String], orthologyMode: OrthologyMode, species: Seq[String]): Map[String, Vector[Orthology]] = {
    val gs = referenceGenes.map(unUri)
    val orthology =
      s"""
         |  values ?orthology { ${if(orthologyMode.one2one) "ens:ortholog_one2one" else ""} ${if(orthologyMode.one2many) "ens:ortholog_one2many" else ""} ${if(orthologyMode.many2many) "ens:ortholog_many2many" else ""}  } .
         |""".stripMargin
    val query = s"""$commonPrefixes
                   |
                   |SELECT * WHERE
                   |{
                   |    values ?species { ${species.map(unUri).mkString(" ")} }
                   |    values ?reference_gene { ${gs.map(unUri).mkString(" ")} } . #put reference genes selected by the user
                   |    GRAPH ?confidence {
                   |      ${orthology}
                   |      ?reference_gene ?orthology ?ortholog .
                   |	}
                   |    ?species :has_gene ?ortholog .
                   |    ?species :has_taxon ?any .
                   |    ?ortholog rdfs:label ?ortholog_symbol
                   |}
                   |ORDER BY ?orthology ?confidence
      """.stripMargin
    get_query(query).map(f=>Orthology(f)).groupBy(f=>f.reference_gene)
  }



  /*
  def get_ortholog_one2one(genes: Vector[String], species: String): Vector[Orthology] = {
    val str = genes.map(unUri).mkString(" ")
    val query = s"""
       |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
       |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
       |PREFIX owl: <http://www.w3.org/2002/07/owl#>
       |PREFIX ens: <http://rdf.ebi.ac.uk/resource/ensembl/>
       |PREFIX samples:<http://aging-research.group/samples/>
       |PREFIX : <http://aging-research.group/resource/>
       |
       |SELECT * WHERE
       |{
       |    values ?reference_gene { ${str} } . #put reference genes selected by the user
       |    GRAPH ?confidence {
       |      values ?orthology { ens:ortholog_one2one  } .
       |      ?reference_gene ?orthology ?ortholog .
       |	}
       |    ${unUri(species)} :has_gene ?ortholog .
       |    ?ortholog rdfs:label ?ortholog_symbol
       |}
      """.stripMargin
      get_query(query).map(f=>Orthology(f))
  }
   */

  def speciesGenes(species: String = "http://aging-research.group/resource/Homo_sapiens"): Vector[String] = {
      val gene = variable("gene")
    val sp = if(species.contains(":")) species else s"http://aging-research.group/resource/${species}"
      val selectQuery: SelectQuery = Queries.SELECT(gene)
      .where(triple(
        sp,
        "http://aging-research.group/resource/has_gene",
        "?gene")
    )
      val results =get_query(selectQuery.getQueryString)
      results.map(v=>v("gene"))
  }



  def shutDown() = {
    repository.shutDown();
    repositoryManager.shutDown();
  }

}
