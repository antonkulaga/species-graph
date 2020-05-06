package species.cli

import java.nio.file.Path

import better.files.File
import com.monovore.decline.{Command, Opts}
import species.sparql.{EnsemblSpecies, Genes, OrthologyMode, Species}
import species.tables.OrthologyTable
import cats.implicits._
import com.monovore.decline._

trait OrthologyCommands {

  lazy val g: Genes = Genes()
  lazy val humanGenes: Vector[String] = g.speciesGenes()
  lazy val animal_classes = Vector(
    "http://rdf.ebi.ac.uk/resource/ensembl/Mammalia",
    "http://rdf.ebi.ac.uk/resource/ensembl/Aves",
    "http://rdf.ebi.ac.uk/resource/ensembl/Reptilia",
    "http://rdf.ebi.ac.uk/resource/ensembl/Teleostei",
    "http://rdf.ebi.ac.uk/resource/ensembl/Chondrichthyes",
    "http://rdf.ebi.ac.uk/resource/ensembl/Coelacanthi",
  )

  lazy val orthologyPath: Opts[String] = Opts.option[String](long = "path", help = "Orthology path").withDefault("/data/species/by_class_and_lifespan")
  lazy val classes: Opts[String] = Opts.option[String](long = "classes", help = "Animal classes").withDefault("all")


  def orthology_tables(path: String, classes: String): Unit = {
    val species: Vector[EnsemblSpecies] = Species.get_species_in_samples()
    val folder = File(path)

    writeGenes(humanGenes, species, folder)
  }

  lazy val orthologs: Command[Unit] = Command(
    name = "orthologs", header = "Generate orthology table"
  ) {
    (orthologyPath, classes).mapN{
      case (path, "all" | "species") =>
        val species: Vector[EnsemblSpecies] = Species.get_species_in_samples()
        val folder = File(path)
        writeGenes(humanGenes, species, folder)

      case (path, "split") =>
        val folder = File(path)
        val speciesGrouped= Species.get_species_in_samples().groupBy(_.animal_class)
        for{
          (cl, sp) <- speciesGrouped
          cl_name = cl
            .replace("http://rdf.ebi.ac.uk/resource/ensembl/", "")
            .replace("<", "").replace(">", "").replace("ens:", "")
        }{
          writeGenes(humanGenes, sp, folder / cl_name)
        }

      case (path, cl) =>
       println("OTHER")
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
