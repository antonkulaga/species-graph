package species.sparql.samples

import species.sparql.QueryBase

import scala.collection.immutable.{ListMap, Vector}

case class EnsemblSpecies(latin_name: String, common_name: String, animal_class: String, lifespan: String, ensembl_url: String, taxon: String)
object Species extends Species("http://10.40.3.21:7200/") {
  def apply(serverURL: String) = new Species(serverURL)
}
class Species(val serverURL: String ="http://10.40.3.21:7200/") extends QueryBase {
 // def mammals_in_samples(): Vector[String] = get_species_in_samples("ens:Mammalia")


  def species(samples: Vector[String] = Vector.empty, project: String = ":Cross_species", na: String = "N/A"): Vector[ListMap[String, String]] = {
  val query =
    s"""$commonPrefixes
       |SELECT DISTINCT ?species ?common_name	?animal_class ?lifespan
       |?ensembl_url ?mass_g	?metabolic_rate ?temperature ?temperature_kelvin ?gestation_days  ?taxon
       |?female_maturity_days ?male_maturity_days ?litters_per_year ?inter_birth_interval ?birth_weight_g ?weaning_weight_g
       | WHERE {
       |      ?species :has_common_name ?common_name .
       |      ?species :has_ensembl_url ?ensembl_url .
       |      ?species :is_animal_class ?animal_class .
       |      ?species :has_lifespan ?lifespan .
       |      OPTIONAL { ?species :has_mass_g ?mass_g . }
       |      OPTIONAL { ?species :has_metabolic_rate ?metabolic_rate . }
       |      ?species :has_taxon ?taxon .
       |      OPTIONAL { ?species :has_temperature ?temperature . }
       |      OPTIONAL { ?species :has_temperature_kelvin ?temperature_kelvin . }
       |      OPTIONAL { ?species :has_gestation_days ?gestation_days . }
       |      OPTIONAL { ?species :has_female_maturity_days ?female_maturity_days . }
       |      OPTIONAL { ?species :has_male_maturity_days ?male_maturity_days . }
       |      OPTIONAL { ?species :has_weaning ?female_maturity_days . }
       |      OPTIONAL { ?species :has_litter_size ?female_maturity_days . }
       |      OPTIONAL { ?species :has_litters_per_year ?litters_per_year . }
       |      OPTIONAL { ?species :has_inter_birth_interval ?inter_birth_interval . }
       |      OPTIONAL { ?species :has_birth_weight_g ?birth_weight_g . }
       |      OPTIONAL { ?species :has_weaning_weight_g ?weaning_weight_g . }
       |	    ?species rdf:type :Species .
       |} ORDER BY ?animal_class DESC(?lifespan)
       |""".stripMargin
        select_query_ordered(query, na)
  }

  def species_for_genes(genes: Vector[String]): Vector[EnsemblSpecies] = {
    val query = s"""${commonPrefixes}
                   |SELECT DISTINCT ?species ?common_name ?animal_class  ?lifespan ?ensembl_url  ?taxon WHERE
                   |{
                   |	?species rdf:type :Species .
                   |    ${values(name = "gene", genes.map(ens))}
                   |    ?species :has_gene ?gene .
                   |    ?species :has_common_name ?common_name .
                   |    ?species :is_animal_class ?animal_class .
                   |    ?species :has_taxon ?taxon .
                   |    ?species :has_lifespan ?lifespan .
                   |    ?species :has_ensembl_url ?ensembl_url .
                   |} ORDER BY ?animal_class DESC(?lifespan)
                   |""".stripMargin
    select_query(query)
      .map(f=>EnsemblSpecies(shorten(f("species")), f("common_name"), shorten(f("animal_class")),  f("lifespan"), f("ensembl_url"),  f("taxon")))
  }
  /**
   * Gets species in the samples if
   * @param samples
   * @return
   */
  def species_in_samples(samples: Vector[String] = Vector.empty): Vector[EnsemblSpecies] = {
    val query = s"""${commonPrefixes}
                   |SELECT DISTINCT ?species ?common_name ?animal_class  ?lifespan ?ensembl_url  ?taxon WHERE
                   |{
                   |	?species rdf:type :Species .
                   |    ${values("run", samples.map(u))}
                   |    ?run samples:has_organism ?species .
                   |    ?species :has_common_name ?common_name .
                   |    ?species :is_animal_class ?animal_class .
                   |    ?species :has_taxon ?taxon .
                   |    ?species :has_lifespan ?lifespan .
                   |    ?species :has_ensembl_url ?ensembl_url .
                   |} ORDER BY ?animal_class DESC(?lifespan)
                   |""".stripMargin
    select_query(query)
      .map(f=>EnsemblSpecies(shorten(f("species")), f("common_name"), shorten(f("animal_class")),  f("lifespan"), f("ensembl_url"),  f("taxon")))
  }

  def species_in_samples_by_class(animal_classes: Vector[String]): Vector[EnsemblSpecies] = {
    val query = s"""${commonPrefixes}
                   |SELECT DISTINCT ?species ?common_name ?animal_class  ?lifespan ?ensembl_url  ?taxon WHERE
                   |{
                   |	?species rdf:type :Species .
                   |    ?run samples:has_organism ?species .
                   |    ?species :has_common_name ?common_name .
                   |    ${values("animal_class", animal_classes.map(u))}
                   |    ?species :is_animal_class ?animal_class .
                   |    ?species :has_taxon ?taxon .
                   |    ?species :has_lifespan ?lifespan .
                   |    ?species :has_ensembl_url ?ensembl_url .
                   |} ORDER BY DESC(?lifespan)
                   |""".stripMargin
    select_query(query)
      .map(f=>EnsemblSpecies(shorten(f("species")), f("common_name"), shorten(f("animal_class")),  f("lifespan"), f("ensembl_url"),  f("taxon")))
  }

}
