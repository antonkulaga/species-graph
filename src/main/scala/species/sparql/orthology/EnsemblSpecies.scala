package species.sparql.orthology

import species.sparql.QueryBase

import scala.collection.immutable.{Map, Vector}
case class EnsemblSpecies(latin_name: String, common_name: String, animal_class: String, lifespan: String, ensembl_url: String, taxon: String)
object Species extends Species("http://10.40.3.21:7200/") {
  def apply(serverURL: String) = new Species(serverURL)
}
class Species(val serverURL: String ="http://10.40.3.21:7200/") extends QueryBase {
  def get_mammals_in_samples(): Vector[String] = get_species_in_samples("ens:Mammalia")

  def get_species_in_samples(): Vector[EnsemblSpecies] = {
    val query = s"""${commonPrefixes}
                   |SELECT DISTINCT ?species ?common_name ?animal_class  ?lifespan ?ensembl_url  ?taxon WHERE
                   |{
                   |	?species rdf:type :Species .
                   |    ?run samples:has_organism ?species .
                   |    ?species :has_common_name ?common_name .
                   |    ?species :is_animal_class ?animal_class .
                   |    ?species :has_taxon ?taxon .
                   |    ?species :has_lifespan ?lifespan .
                   |    ?species :has_ensembl_url ?ensembl_url .
                   |} ORDER BY DESC(?lifespan)
                   |""".stripMargin
    select_query(query)
      .map(f=>EnsemblSpecies(f("species"), f("common_name"), f("animal_class"),  f("lifespan"), f("ensembl_url"),  f("taxon")))
  }

  def get_species_in_samples(animal_class: String): Vector[String] = {
    val query = s"""${commonPrefixes}
                   |SELECT DISTINCT ?species WHERE
                   |{
                   |    ?run samples:has_organism ?species .
                   |    ?species :is_animal_class ${animal_class} .
                   |} ORDER BY ?species
                   |""".stripMargin
    select_query(query).map(f=>f("species"))
  }

  def get_species_in_samples_by_class(): Map[String, Vector[String]] = {
    val query = s"""${commonPrefixes}
                  |SELECT DISTINCT ?species ?class WHERE
                  |{
                  |    ?run samples:has_organism ?species .
                  |    ?species :is_animal_class ?class
                  |} ORDER BY ?class ?species
                  |""".stripMargin
    select_query(query).groupBy(mp=>mp("class")).map{ case (k, v)=>k->v.map(_("species"))}
  }

}
