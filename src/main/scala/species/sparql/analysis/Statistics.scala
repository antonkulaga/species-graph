package species.sparql.analysis

import species.sparql.QueryBase
import species.sparql.samples.EnsemblSpecies

import scala.util._
object Statistics extends Statistics("http://10.40.3.21:7200/")
class Statistics(val serverURL: String = "http://10.40.3.21:7200/") extends QueryBase {

  def writeManySpecies(species: Vector[EnsemblSpecies], orthology: String = "ens:ortholog_one2many") = {
    for(s <- species)
      {
        val sp = s.latin_name.replace( "http://aging-research.group/resource/", ":")
        print(s"WRITING ${sp} stats with ${orthology}")
        writeSpecies(sp, orthology) match {
          case Failure(exception) => println(s"failed ${sp} with ${exception.toString}")
          case Success(_) =>
            println(s"finished writing ${orthology} for ${sp}")
        }

      }
    println("FINISHED ALL THE SPECIES")
  }

  def writeSpecies(species: String, orthology: String = "ens:ortholog_one2many") = {
    val query = s"""
      |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      |PREFIX ens: <http://rdf.ebi.ac.uk/resource/ensembl/>
      |PREFIX : <http://aging-research.group/resource/>
      |PREFIX samples: <http://aging-research.group/samples/>
      |PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
      |
      |INSERT { GRAPH <http://aging-research.group/statistics/> {?reference_gene ?property  ?count .} }
      |#SELECT ?reference_gene ?property  ?count
      |WHERE
      |{
      |    BIND(
      |        IRI(replace(STR(?species),"http://aging-research.group/resource/", "http://aging-research.group/resource/one2many_count_in_") )AS ?property)
      |    {
      |        SELECT  ?reference_gene ?species (COALESCE(COUNT(?ortholog)) as ?count) WHERE
      |    {
      |        values ?reference_species { ${(":"+species).replace("::", ":")} } .
      |        ?reference_species :has_gene ?reference_gene .
      |        ?reference_gene ${("ens:" + orthology).replace("ens:ens:", "ens:")} ?ortholog .
      |        ?species :has_gene ?ortholog .
      |        ?species rdf:type :Species .
      |    } group by ?reference_gene ?species HAVING (COUNT(?ortholog) > 0)
      |    }
      |}
      |""".stripMargin
      Try(this.insert_query(query))
  }
}
