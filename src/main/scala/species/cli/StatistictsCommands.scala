package species.cli

import com.monovore.decline._
import java.nio.file.Path

import better.files.File
import com.monovore.decline.{Command, Opts}
import cats.implicits._
import com.monovore.decline._
import species.sparql.analysis.Statistics
import species.sparql.orthology.OrthologyTable
import species.sparql.samples.Species

import scala.util.{Failure, Success}

trait StatistictsCommands {

  lazy val species: Opts[String] = Opts.option[String](long = "species", help = "Species").withDefault("Homo_sapiens")
  lazy val orthology: Opts[String] = Opts.option[String](long = "orthology", help = "Orthology relation, i.e. ens:ortholog_one2many").withDefault("ens:ortholog_one2many")

  lazy val genes_statistics: Command[Unit] = Command(
    name = "genes_statistics", header = "Writed count statistics"
  ) {
    (species, orthology).mapN{
      case ("all", o) =>
        println(s"WRITING MANY statis with ${o} orthology")
        val sps = Species.species_in_samples()
        Statistics.writeManySpecies(sps, o)

      case ("all_but_mousehuman", o) =>
        println(s"WRITING MANY statistics with ${o} orthology")
        val sps = Species.species_in_samples().filterNot(s=> s.latin_name.contains("Homo_sapiens") || s.latin_name.contains("Mus_musculus"))
        //pprint.pprintln(sps)
      Statistics.writeManySpecies(sps, o)


      case (sp, o)  =>
        print(s"WRITING ${sp} stats with ${orthology}")
        Statistics.writeSpecies(sp, o) match {
          case Failure(exception) => println(s"failed ${sp} with ${exception.toString}")
          case Success(_) =>
            println(s"finished writing ${orthology} for ${sp}")
        }

    }
  }


}
