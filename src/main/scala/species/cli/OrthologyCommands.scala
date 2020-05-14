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

  lazy val animal_classes = Vector(
    "http://rdf.ebi.ac.uk/resource/ensembl/Mammalia",
    "http://rdf.ebi.ac.uk/resource/ensembl/Aves",
    "http://rdf.ebi.ac.uk/resource/ensembl/Reptilia",
    "http://rdf.ebi.ac.uk/resource/ensembl/Teleostei",
    "http://rdf.ebi.ac.uk/resource/ensembl/Chondrichthyes",
    "http://rdf.ebi.ac.uk/resource/ensembl/Coelacanthi",
  )

  lazy val genes: Opts[String] = Opts.option[String](long = "genes", help = "reference genes: either list of exact genes or the species names to take all genes from (default Homo_sapiens)").withDefault("Homo_sapiens")



  lazy val orthologyPath: Opts[String] = Opts.option[String](long = "path", help = "Folder to store orthology tables")

  lazy val separator: Opts[String] = Opts.option[String](long = "sep", help = "TSV/CSV separator ( \t by default)").withDefault("\t")

  //.withDefault("/data/species/by_class_and_lifespan")

  //lazy val classes: Opts[String] = Opts.option[String](long = "classes", help = "Animal classes").withDefault("all")

  lazy val split: Opts[SplitGenes] = Opts.option[SplitGenes](long = "split", help = "How to split files (nosplit, byclass, byspecies)").withDefault(SplitGenes.NoSplit)

  lazy val server: Opts[String] = Opts.option[String](long = "server", help = "URL of GraphDB server, default = http://10.40.3.21:7200").withDefault("http://10.40.3.21:7200")

  lazy val slide: Opts[Int] = Opts.option[Int](long = "slide", help = "retrieves genes by batches (of 2000 by default)").withDefault(2000)


  protected def sep(str: String) = if(str.contains(",")) "," else ";"

  lazy val na: Opts[String] = Opts.option[String](long = "na", help = "What to write when data is not availible (default N/A)").withDefault("N/A")
  lazy val verbose: Opts[Boolean] = Opts.flag(long = "verbose", "Includes additional information for debuging (i.e. gene id-s)").orFalse
  lazy val rewrite: Opts[Boolean] = Opts.flag(long = "rewrite", "if output folder exists then cleans it before writing").orFalse


  /**
   * Extracts genes from command line parameters
   * @param gs
   * @param orthologyManager
   * @return
   */
  protected def extract_genes(gs: String)(implicit orthologyManager: OrthologyManager): Vector[String] = {
    if(gs.toLowerCase.contains("homo_sapiens")) orthologyManager.speciesGenes() else {
      if(gs.toLowerCase().contains("ens") || gs.contains(";") || gs.contains(",")) {
        gs.split(sep(gs)).map(g=> orthologyManager.ens(g)).toVector
      } else
        orthologyManager.speciesGenes((":"+gs).replace("::", ":"))
    }

  }

  def write_orthologs_implementation(path: String, split:SplitGenes, server: String, genes: String) = (path, split, server, genes) match {

    case (path, SplitGenes.NoSplit, server, gs) =>
      val species: Vector[EnsemblSpecies] = new Species(server).species_in_samples()
      implicit val orthologyManager = new OrthologyManager(server)
      val reference_genes = extract_genes(gs)

      val folder = File(path)

      writeGenes(reference_genes, species, folder)

    case (path, SplitGenes.ByClass, server, gs) =>

      val folder = File(path)
      implicit val orthologyManager = new OrthologyManager(server)
      val reference_genes = extract_genes(gs)
      val speciesGrouped= new Species(server).species_in_samples().groupBy(_.animal_class)
      for{
        (cl, sp) <- speciesGrouped
        cl_name = cl
          .replace("http://rdf.ebi.ac.uk/resource/ensembl/", "")
          .replace("<", "").replace(">", "").replace("ens:", "")
      }{
        writeGenes(reference_genes, sp, folder / cl_name)
      }

    case (path, SplitGenes.BySpecies, server, gs) =>
      println("Split by species not implemented yet")
  }

  lazy val orthologs: Command[Unit] = Command(
    name = "orthologs", header = "Generate orthology tables"
  ) {
    (orthologyPath, split, server, genes).mapN(write_orthologs_implementation)
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
