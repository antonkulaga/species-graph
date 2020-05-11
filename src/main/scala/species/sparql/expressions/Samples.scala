package species.sparql.expressions

import species.sparql.QueryBase

import scala.collection.immutable._
import scala.util.Try

case class SampleMini(run: String, species: String, tissue: String, lifespan: Double, animal_class: String)
object Samples extends Samples("http://10.40.3.21:7200/")
class Samples(val serverURL: String = "http://10.40.3.21:7200/") extends QueryBase{

  def get_stats_by_species(): Map[String, Map[String, Int]] = {
    val mp = get_all_samples_mini().groupBy(s=>s.species)
    mp.map{ case (k, v) => k->v.groupBy(sv=>sv.tissue).map{ case (k, v)=>k->v.length} }
  }

  def get_stats_by_tissue(): Map[String, Map[String, Int]] = {
    val mp = get_all_samples_mini().groupBy(s=>s.tissue)
    mp.map{ case (k, v) => k->v.groupBy(sv=>sv.tissue).map{ case (k, v)=>k->v.length} }
  }

  def get_samples_by_tissue(tissue: String, project: String = ":Cross-species"): Vector[SampleMini] = {
    val query = s"""${commonPrefixes}
                   |
                   |SELECT * WHERE
                   |{
                   |    ?run samples:has_organism ?species .
                   |    ?run samples:used_in_project ${unUri(project)} . #defines the project
                   |    ?run samples:of_tissue ${unUri(tissue)} . #gets tissue
                   |    ?species :has_lifespan ?lifespan .
                   |    ?species :has_lifespan ?lifespan .
                   |} ORDER BY ?species ?bioproject ?series ?run
                   |""".stripMargin
    select_query(query).map(mp => SampleMini(mp("run"), mp("species"), mp("tissue"),Try(mp("lifespan").toDouble).getOrElse(Double.NaN), mp("animal_class")))
  }

  def get_all_samples_mini(project: String = ":Cross-species"): Vector[SampleMini] = {
    val query = s"""${commonPrefixes}
      |
      |SELECT * WHERE
      |{
      |    ?run samples:has_organism ?species . #species
      |    ?run samples:used_in_project ${unUri(project)} . #defines the project
      |    ?run samples:of_tissue ?tissue . #gets tissue
      |    ?species :is_animal_class ?animal_class .
      |} ORDER BY ?species ?bioproject ?series ?run
      |""".stripMargin
    select_query(query).map(mp => SampleMini(mp("run"), mp("species"), mp("tissue"),Try(mp("lifespan").toDouble).getOrElse(Double.NaN), mp("animal_class")))
  }

  def get_all_samples_mini_by_runs(runs: Seq[String], project: String = ":Cross-species"): Vector[SampleMini] = {
    val query = s"""${commonPrefixes}
                   |
                   |SELECT * WHERE
                   |{
                   |    values ?run ${runs.map(r=>this.sra(r)).mkString(" ")}
                   |    ?run samples:has_organism ?species . #species
                   |    ?run samples:used_in_project ${unUri(project)} . #defines the project
                   |    ?run samples:of_tissue ?tissue . #gets tissue
                   |    ?species :is_animal_class ?animal_class .
                   |} ORDER BY ?species ?bioproject ?series ?run
                   |""".stripMargin
    select_query(query).map(mp => SampleMini(mp("run"), mp("species"), mp("tissue"),Try(mp("lifespan").toDouble).getOrElse(Double.NaN), mp("animal_class")))
  }

  def get_all_samples(project: String = ":Cross-species"): Vector[ListMap[String, String]] = {
   val query =  s"""${commonPrefixes}
      |SELECT * WHERE
      |{
      |    ?bioproject rdf:type samples:Bioproject . #gets all bioprojects
      |    ?bioproject samples:has_series ?series .
      |    ?series samples:has_run ?run . #gets sequencing runs from experimental series
      |    ?run samples:has_organism ?species . #species
      |    ?run samples:used_in_project ${project} . #defines the project
      |    ?run samples:of_tissue ?tissue . #gets tissue
      |    ?run samples:has_sample_name ?sample_name .
      |    ?run samples:has_characteristics ?characterists .
      |    ?run samples:has_sequencer ?sequencer .
      |    ?run samples:has_age ?age .
      |    ?run samples:has_sex ?sex .
      |    ?run samples:has_tumor ?tumor .
      |    ?run samples:has_source ?source .
      |    ?run samples:has_study ?study .
      |    ?run samples:has_study_title ?study_title .
      |    ?run samples:has_salmon_version ?salmon_version .
      |    ?run samples:has_library_layout ?library_layout .
      |    ?run samples:has_library_selection ?library_selection .
      |    ?run samples:has_library_strategy ?library_strategy .
      |    ?run samples:has_libType ?lib_type .
      |    ?run samples:has_numBootstraps ?bootstrap .
      |    ?run samples:has_modified ?modified .
      |    ?run samples:has_protocol ?protocol .
      |} ORDER BY ?species ?bioproject ?series ?run
      |""".stripMargin
    select_query(query)
  }

  def get_mammalian_samples_mini(): Vector[SampleMini] = get_samples_mini_by_class("ens:Mammalia")

  def get_samples_mini_by_class(animal_class: String, project: String = ":Cross-species"): Vector[SampleMini] = {
    val query = s"""${commonPrefixes}
                   |
                   |SELECT * WHERE
                   |{
                   |    ?run samples:has_organism ?species .
                   |    ?run samples:used_in_project ${project} . #defines the project
                   |    ?run samples:of_tissue ?tissue . #gets tissue
                   |    ?species :is_animal_class ${animal_class} .
                   |    ?species :has_lifespan ?lifespan .
                   |} ORDER BY ?species ?bioproject ?series ?run
                   |""".stripMargin
    select_query(query).map(mp => SampleMini(mp("run"), mp("species"), mp("tissue"),Try(mp("lifespan").toDouble).getOrElse(Double.NaN), mp("animal_class")))
  }


}
