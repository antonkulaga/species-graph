package species.cli

import java.nio.file.Path

import better.files.File
import com.monovore.decline.{Command, Opts}
import cats.implicits._
import com.monovore.decline._
import _root_.enumeratum.{Enum, EnumEntry}
import com.monovore.decline.enumeratum._
import species.sparql.orthology.{OrthologyManager, OrthologyMode, OrthologyTable}
import species.sparql.samples.{Samples, Species}

trait SamplesCommands {

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

  //lazy val samples_path: Opts[String] = Opts.option[String](long = "samples_path", help = "Where to store samples jnfo")
  //lazy val species_path: Opts[String] = Opts.option[String](long = "species_path", help = "Where to store species info")
  lazy val output: Opts[String] = Opts.option[String](long = "output", help = "Where to write output")

  lazy val all_species: Opts[Boolean] = Opts.flag(long = "all", "if we should include all species (incl. those for which we have no samples) to the index (false by default").orFalse

  lazy val project: Opts[String] = Opts.option[String](long = "project", help = "which project do we take samples from (:Cross-species by default)").withDefault(":Cross-species")

  protected def sep(str: String) = if(str.contains(",")) "," else ";"

  lazy val samples_index: Command[Unit] = Command(
    name = "samples_index", header = "Generates list of samples"
  ) {
    (output, server, project, separator).mapN(write_samples)
  }

  protected def write_samples(path: String, server: String, project: String = ":Cross-species", sep: String):Unit = {
    val s = new Samples(server)
    val samples = s.samples_full(project)
    if(samples.nonEmpty){
      val f = File(path)
      if(f.exists) {
        println("output file " + path + " exists, deleting it to write new output")
        f.delete()
      }
      f.createFileIfNotExists(true)
      f.appendLine(samples.head.keys.toVector.mkString(sep))
      for(s<-samples) f.appendLine(s.values.toVector.mkString(sep))
      println(s"FINISHED WRITING SAMPLES TO ${f.pathAsString}")
    } else println("NO SAMPLES FOUND!")

  }

/*
  lazy val species_index: Command[Unit] = Command(
    name = "orthologs", header = "Generate orthology tables"
  ) {
    (species_path, server, all_species).mapN(write_orthologs_implementation)
  }

  def write_species(path: String, server: String, all: Boolean = false): Unit ={
    val sp = new Species(server)
    //if(all) sp.species_in_samples() else sp.species_in_samples()
    val species = sp.species_in_samples()
    if(samples.nonEmpty){
      val f = File(path)
      if(f.exists) {
        println("output file " + path + " exists, deleting it to write new output")
        f.delete()
      }
      f.createFileIfNotExists(true)
      f.appendLine(samples.head.keys.toVector.mkString(sep))
      for(s<-samples) f.appendLine(s.values.toVector.mkString(sep))
      println(s"FINISHED WRITING SAMPLES TO ${f.pathAsString}")
    } else println("NO SAMPLES FOUND!")
  }
 */

}
