package species.cli

import java.nio.file.Path

import better.files.File
import com.monovore.decline.{Command, Opts}
import cats.implicits._
import com.monovore.decline._
import _root_.enumeratum.{Enum, EnumEntry}
import com.monovore.decline.enumeratum._
import species.sparql.orthology.{EnsemblSpecies, OrthologyManager, OrthologyMode, OrthologyTable, Species}

sealed trait SplitGenes extends EnumEntry with EnumEntry.Lowercase

object SplitGenes extends Enum[SplitGenes] {
  case object NoSplit extends SplitGenes
  case object ByClass extends SplitGenes
  case object BySpecies extends SplitGenes

  val values = findValues
}

trait OrthologyCommands {

  lazy val g: OrthologyManager = OrthologyManager()
  lazy val humanGenes: Vector[String] = g.speciesGenes()
  lazy val animal_classes = Vector(
    "http://rdf.ebi.ac.uk/resource/ensembl/Mammalia",
    "http://rdf.ebi.ac.uk/resource/ensembl/Aves",
    "http://rdf.ebi.ac.uk/resource/ensembl/Reptilia",
    "http://rdf.ebi.ac.uk/resource/ensembl/Teleostei",
    "http://rdf.ebi.ac.uk/resource/ensembl/Chondrichthyes",
    "http://rdf.ebi.ac.uk/resource/ensembl/Coelacanthi",
  )

  lazy val orthologyPath: Opts[String] = Opts.option[String](long = "path", help = "Folder to store orthology tables")
    //.withDefault("/data/species/by_class_and_lifespan")

  //lazy val classes: Opts[String] = Opts.option[String](long = "classes", help = "Animal classes").withDefault("all")




  lazy val split: Opts[SplitGenes] = Opts.option[SplitGenes](long = "split", help = "How to split files (nosplit, byclass, byspecies)").withDefault(SplitGenes.NoSplit)


  lazy val server: Opts[String] = Opts.option[String](long = "server", help = "URL of GraphDB server, default = http://10.40.3.21:7200").withDefault("http://10.40.3.21:7200")


  def orthology_tables(path: String, classes: String): Unit = {
    val species: Vector[EnsemblSpecies] = Species.get_species_in_samples()
    val folder = File(path)

    writeGenes(humanGenes, species, folder)
  }

  lazy val orthologs: Command[Unit] = Command(
    name = "orthologs", header = "Generate orthology tables"
  ) {
    (orthologyPath, split, server).mapN{
      case (path, SplitGenes.NoSplit, server) =>
        val species: Vector[EnsemblSpecies] = new Species(server).get_species_in_samples()
        val folder = File(path)
        writeGenes(humanGenes, species, folder)

      case (path, SplitGenes.ByClass, server) =>
        val folder = File(path)
        val speciesGrouped= new Species(server).get_species_in_samples().groupBy(_.animal_class)
        for{
          (cl, sp) <- speciesGrouped
          cl_name = cl
            .replace("http://rdf.ebi.ac.uk/resource/ensembl/", "")
            .replace("<", "").replace(">", "").replace("ens:", "")
        }{
          writeGenes(humanGenes, sp, folder / cl_name)
        }

      case (path, SplitGenes.BySpecies, server) =>
       println("Split by species not implemented yet")
    }
  }



  def writeGenes(genes: Vector[String], species: Vector[EnsemblSpecies], folder: File): Unit = {
    println(s"writing orthology table for ${folder.pathAsString}")
    folder.createDirectoryIfNotExists()
    val orthologyTable = new OrthologyTable(species)
    orthologyTable.writeOrthology(genes,  OrthologyMode.one2one, (folder / "one2one.tsv").pathAsString)
    println("ONE2ONE finished")
    orthologyTable.writeOrthology(genes,  OrthologyMode.one2many , (folder / "one2many.tsv").pathAsString)
    println("ONE2MANY finished")
    orthologyTable.writeOrthology(genes,  OrthologyMode.one2many_directed , (folder / "one2many_directed.tsv").pathAsString)
    println("ONE2MANY directed finished")
    orthologyTable.writeOrthology(genes,  OrthologyMode.many2many,  (folder / "many2many.tsv").pathAsString)
    println("MANY2MANY finished")
    orthologyTable.writeOrthology(genes,  OrthologyMode.all , (folder / "all.tsv").pathAsString)
    println("ALL finished")
    println(s"writing orthology table for ${folder.pathAsString} FINISHED")
    println("-----------------------")
  }
}
