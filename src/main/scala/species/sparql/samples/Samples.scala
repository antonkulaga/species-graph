package species.sparql.samples

import species.sparql.QueryBase

import scala.collection.immutable._
import scala.util.Try

case class SampleMini(run: String, species: String, tissue: String, lifespan: Double, animal_class: String)
object Samples extends Samples("http://10.40.3.21:7200/")
class Samples(val serverURL: String = "http://10.40.3.21:7200/") extends QueryBase{

  def get_stats_by_species(): Map[String, Map[String, Int]] = {
    val mp = samples_mini_by_runs().groupBy(s=>s.species)
    mp.map{ case (k, v) => k->v.groupBy(sv=>sv.tissue).map{ case (k, v)=>k->v.length} }
  }

  def get_stats_by_tissue(): Map[String, Map[String, Int]] = {
    val mp = samples_mini_by_runs().groupBy(s=>s.tissue)
    mp.map{ case (k, v) => k->v.groupBy(sv=>sv.tissue).map{ case (k, v)=>k->v.length} }
  }

  def get_samples_by_tissue(tissue: String, project: String = ":Cross-species"): Vector[SampleMini] = {
    val query = s"""${commonPrefixes}
                   |
                   |SELECT * WHERE
                   |{
                   |    ?run samples:has_organism ?species .
                   |    ?run samples:used_in_project ${u(project)} . #defines the project
                   |    ?run samples:of_tissue ${u(tissue)} . #gets tissue
                   |    ?species :has_lifespan ?lifespan .
                   |} ORDER BY ?species ?bioproject ?series ?run
                   |""".stripMargin
    select_query(query).map(mp => SampleMini(shorten(mp("run")), shorten(mp("species")), shorten(mp("tissue")),
      Try(mp("lifespan").toDouble).getOrElse(Double.NaN), shorten(mp("animal_class"))))
  }

  /**
   * Gets samples mini
   * @param runs
   * @param project
   * @return
   */
  def samples_mini_by_runs(runs: Seq[String] = Seq.empty,project: String = ":Cross-species"): Vector[SampleMini] = {
    val runsStr = values("run", runs.map(r=>this.sra(r)))
    val query = s"""${commonPrefixes}
                   |SELECT * WHERE
                   |{
                   |    $runsStr
                   |    ?run samples:has_organism ?species . #species
                   |    ?run samples:used_in_project ${u(project)} . #defines the project
                   |    ?run samples:of_tissue ?tissue . #gets tissue
                   |    ?species :is_animal_class ?animal_class .
                   |    ?species :has_lifespan ?lifespan .
                   |} ORDER BY ?animal_class ?lifespan ?bioproject ?series ?run
                   |""".stripMargin
    val res = select_query(query)
    res.map(mp => SampleMini(
      shorten(mp("run")), shorten(mp("species")), shorten(mp("tissue")),Try(mp("lifespan").toDouble).getOrElse(Double.NaN), shorten(mp("animal_class"))))
  }

  def samples_full(project: String = ":Cross-species", na: String = "N/A"): Vector[ListMap[String, String]] = {
   val query =  s"""${commonPrefixes}
      |SELECT ?bioproject ?series ?run ?species ?tissue ?sample_name ?characteristics
      |?sequencer ?age ?sex ?tumor ?source ?study ?study_title
      |?salmon_version ?library_layout ?library_selection
      |?library_strategy ?lib_type ?bootstrap ?modified ?protocol
      |WHERE
      |{
      |    ?bioproject rdf:type samples:Bioproject . #gets all bioprojects
      |    ?bioproject samples:has_series ?series .
      |    ?series samples:has_run ?run . #gets sequencing runs from experimental series
      |    ?run samples:has_organism ?species . #species
      |    ?run samples:used_in_project ${project} . #defines the project
      |    ?run samples:of_tissue ?tissue . #gets tissue
      |    ?run samples:has_sample_name ?sample_name .
      |    ?run samples:has_characteristics ?characteristics .
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
    val result = select_query_ordered(query, na)
    result
  }

  def get_mammalian_samples_mini(): Vector[SampleMini] = samples_mini_by_class_and_tissue("ens:Mammalia")

  /**
   *
   * @param animal_class animal class (loads all if not set)
   * @param tissue animal tissue (loads all if not set)
   * @param project
   * @return
   */
  def samples_mini_by_class_and_tissue(animal_class: String ="", tissue: String ="", project: String = ":Cross-species"): Vector[SampleMini] = {
    val animal_class_str = values("animal_class", animal_class)
    val tissue_str = values("tissue", tissue)
    val query = s"""${commonPrefixes}
                   |
                   |SELECT * WHERE
                   |{
                   |    ${animal_class_str}
                   |    ${tissue_str}
                   |    ?run samples:has_organism ?species .
                   |    ?run samples:used_in_project ${project} . #defines the project
                   |    ?run samples:of_tissue ?tissue . #gets tissue
                   |    ?species :is_animal_class ?animal_class .
                   |    ?species :has_lifespan ?lifespan .
                   |} ORDER BY ?species ?bioproject ?series ?run
                   |""".stripMargin
    select_query(query).map(mp => SampleMini(
      shorten(mp("run")),
      shorten(mp("species")),
      shorten(mp("tissue")),
      Try(mp("lifespan").toDouble).getOrElse(Double.NaN),
      shorten(mp("animal_class"))))
  }


}
