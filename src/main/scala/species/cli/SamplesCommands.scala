package species.cli

import java.util.concurrent.TimeUnit

import better.files.File
import cats.implicits._
import com.monovore.decline.{Command, Opts}
import species.sparql.{Extras, Prefixes}
import species.sparql.samples.{Samples, Species}
import wvlet.log.LogSupport

import scala.collection.immutable.ListMap

trait SamplesCommands extends LogSupport with Extras{

  def time[R](block: => R): R = {
    val start = System.nanoTime()
    val result = block    // call-by-name
    val end = System.nanoTime()
    val difference = end - start
    info("Total execution time: " +
      TimeUnit.NANOSECONDS.toHours(difference) + " hours " +
      ( TimeUnit.NANOSECONDS.toMinutes(difference) -  TimeUnit.HOURS.toMinutes(TimeUnit.NANOSECONDS.toHours(difference)))   + " min " +
      ( TimeUnit.NANOSECONDS.toSeconds(difference) -  TimeUnit.MINUTES.toSeconds(TimeUnit.NANOSECONDS.toMinutes(difference))) + " sec " +
      " - " + difference + " nSec (Total)")
    result
  }


  lazy val animal_classes = Vector(
    "http://rdf.ebi.ac.uk/resource/ensembl/Mammalia",
    "http://rdf.ebi.ac.uk/resource/ensembl/Aves",
    "http://rdf.ebi.ac.uk/resource/ensembl/Reptilia",
    "http://rdf.ebi.ac.uk/resource/ensembl/Teleostei",
    "http://rdf.ebi.ac.uk/resource/ensembl/Chondrichthyes",
    "http://rdf.ebi.ac.uk/resource/ensembl/Coelacanthi",
  )

  lazy val server: Opts[String] = Opts.option[String](long = "server", help = "URL of GraphDB server, default = http://10.40.3.21:7200").withDefault("http://10.40.3.21:7200")
  lazy val separator: Opts[String] = Opts.option[String](long = "sep", help = "TSV/CSV separator ( \t by default)").withDefault("\t")
  lazy val na: Opts[String] = Opts.option[String](long = "na", help = "What to write when data is not availible (default N/A)").withDefault("N/A")
  lazy val rewrite: Opts[Boolean] = Opts.flag(long = "rewrite", "if output folder exists then cleans it before writing").orFalse
  lazy val prefixed: Opts[Boolean] = Opts.flag(long = "prefixed", "Keeps prefixes in the output").orFalse
  lazy val with_empty_rows: Opts[Boolean] = Opts.flag(long = "with_empty_rows", "if we should write empty rows").orFalse


  //lazy val samples_path: Opts[String] = Opts.option[String](long = "samples_path", help = "Where to store samples jnfo")
  //lazy val species_path: Opts[String] = Opts.option[String](long = "species_path", help = "Where to store species info")
  lazy val output: Opts[String] = Opts.option[String](long = "output", help = "File or Folder to write output")

  lazy val all_species: Opts[Boolean] = Opts.flag(long = "all", "if we should include all species (incl. those for which we have no samples) to the index (false by default").orFalse

  lazy val project: Opts[String] = Opts.option[String](long = "project", help = "which project do we take samples from (:Cross-species by default)").withDefault(":Cross-species")


  def simple_query_write(query: Vector[ListMap[String, String]], path: String, sep: String, na: String, no_prefix: Boolean, rewrite: Boolean) = {
    val f = File(path)
    if(f.exists && rewrite) {
      warn("output file " + f.pathAsString + " exists, deleting it to write new output")
      f.delete()
    }
    f.createFileIfNotExists(true)
    val keys = query.head.keys.toVector
    f.appendLine(keys.mkString(sep))
    for(s<-query) {
      val l = (for(k<-keys) yield {
        val str: String = s.get(k).map(v=>if(v=="") na else Prefixes.shorten(v)).getOrElse(na)
        up(str)(no_prefix)
      }).mkString(sep)
      f.appendLine(l)
    }
    f
  }

  lazy val samples_index: Command[Unit] = Command(
    name = "samples_index", header = "Generates list of samples"
  ) {
    (output, server, project, separator, na, prefixed).mapN(write_samples)
  }
  protected def write_samples(path: String, server: String, project: String = ":Cross-species", sep: String, na: String, with_prefix: Boolean):Unit = time{
    val s = new Samples(server)
    val samples = s.samples_full(project)
    if(samples.nonEmpty){
      val f =simple_query_write(samples, path, sep, na,  !with_prefix, true)
      info(s"FINISHED WRITING SAMPLES TO ${f.pathAsString}")
    } else warn("NO SAMPLES FOUND!")

  }


  lazy val species_index: Command[Unit] = Command(
    name = "species_index", header = "Generate species index"
  ) {
    (output, server, separator, na, prefixed).mapN(write_species)
  }

  def write_species(path: String, server: String, sep: String, na: String, with_prefix: Boolean): Unit ={
    val sp = new Species(server)
    //if(all) sp.species_in_samples() else sp.species_in_samples()
    val species = sp.species()
    if(species.nonEmpty){
      val f = simple_query_write(species, path, sep ,na, !with_prefix, true)
    } else println("NO SPECIES FOUND IN SAMPLES!")
  }


}
