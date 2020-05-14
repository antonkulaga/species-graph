package species.cli

import java.nio.file.Path

import better.files.File
import com.monovore.decline.{Command, Opts}
import cats.implicits._
import com.monovore.decline._
import _root_.enumeratum.{Enum, EnumEntry}
import com.monovore.decline.enumeratum._
import species.sparql.orthology.{OrthologyManager, OrthologyMode, OrthologyTable}
import species.sparql.samples.{EnsemblSpecies, Species}

sealed trait Confidence extends EnumEntry with EnumEntry.Lowercase

object Confidence extends Enum[Confidence] {
  case object high extends Confidence
  case object low extends Confidence
  case object all extends Confidence

  val values = findValues
}


sealed trait SplitGenes extends EnumEntry with EnumEntry.Lowercase

object SplitGenes extends Enum[SplitGenes] {
  case object NoSplit extends SplitGenes
  case object ByClass extends SplitGenes
  case object BySpecies extends SplitGenes

  val values = findValues
}


trait OrthologyCommands extends SamplesCommands {

  lazy val genes: Opts[String] = Opts.option[String](long = "genes", help = "reference genes: either list of exact genes or the species names to take all genes from (default :Homo_sapiens)").withDefault(":Homo_sapiens")

  lazy val orthologyPath: Opts[String] = Opts.option[String](long = "path", help = "Folder to store orthology tables")

  lazy val confidence: Opts[Confidence] = Opts.option[Confidence](long = "confidence", help = "How confident are we in orthologs").withDefault(Confidence.all)


  lazy val split: Opts[SplitGenes] = Opts.option[SplitGenes](long = "split", help = "How to split files (no_split, by_class, by_species)").withDefault(SplitGenes.NoSplit)

  lazy val slide: Opts[Int] = Opts.option[Int](long = "slide", help = "splits genes into batches of <slide> genes (20000 by default)").withDefault(20000)

  lazy val verbose: Opts[Boolean] = Opts.flag(long = "verbose", "Includes additional information for debuging (i.e. gene id-s)").orFalse


  /**
   * Extracts genes from command line parameters
   * @param gs
   * @param orthologyManager
   * @return
   */
  protected def extract_genes(gs: String)(implicit orthologyManager: OrthologyManager): Vector[String] = {
    if(gs.startsWith(":") && gs.contains("_")) {
      val result = orthologyManager.speciesGenes(gs)
      println(s"using all ${result.size} genes of ${gs}")
      result
    } else {
      if(gs.toLowerCase().contains("ens") || gs.contains(";") || gs.contains(",")) {
        gs.split(sep(gs)).map(g=> orthologyManager.ens(g)).toVector
      } else
        orthologyManager.speciesGenes((":"+gs).replace("::", ":"))
    }

  }

  def write_orthologs_implementation(path: String, split:SplitGenes, server: String, genes: String, sl: Int) =
    (path, split, server, genes) match {

    case (path, SplitGenes.NoSplit, server, gs) =>
      val species: Vector[EnsemblSpecies] = new Species(server).species_in_samples()
      implicit val orthologyManager = new OrthologyManager(server)
      val reference_genes = extract_genes(gs)

      val folder = File(path)

      writeGenes(reference_genes, species, folder, sl)

    case (path, split, server, gs) if split ==  SplitGenes.ByClass | split == SplitGenes.BySpecies =>

      val folder = File(path)
      implicit val orthologyManager = new OrthologyManager(server)
      val reference_genes = extract_genes(gs)
      val speciesGrouped= new Species(server).species_in_samples().groupBy(s=> split match {
        case SplitGenes.ByClass => s.animal_class.replace("ens:", "").replace(":", "")
        case SplitGenes.BySpecies => s.latin_name.replace("ens:", "").replace(":", "")
      })
      for{
        (cl, sp) <- speciesGrouped
        cl_name = cl
          .replace("http://rdf.ebi.ac.uk/resource/ensembl/", "")
          .replace("<", "").replace(">", "").replace("ens:", "")
      }{
        writeGenes(reference_genes, sp, folder / cl_name, sl)
      }


  }

  lazy val orthologs: Command[Unit] = Command(
    name = "orthologs", header = "Generate orthology tables"
  ) {
    (orthologyPath, split, server, genes, slide).mapN(write_orthologs_implementation)
  }



  def writeGenes(genes: Vector[String], species: Vector[EnsemblSpecies], folder: File, slide: Int): Unit = {
    println(s"writing orthology table for ${folder.pathAsString}")
    folder.createDirectoryIfNotExists()
    val orthologyTable = new OrthologyTable(species)
    orthologyTable.writeOrthology(genes,  OrthologyMode.one2one, (folder / "one2one.tsv").pathAsString, sl = slide)
    println("ONE2ONE finished")
    orthologyTable.writeOrthology(genes,  OrthologyMode.one2many , (folder / "one2many.tsv").pathAsString, sl = slide)
    println("ONE2MANY finished")
    orthologyTable.writeOrthology(genes,  OrthologyMode.one2many_directed , (folder / "one2many_directed.tsv").pathAsString, sl = slide)
    println("ONE2MANY directed finished")
    orthologyTable.writeOrthology(genes,  OrthologyMode.many2many,  (folder / "many2many.tsv").pathAsString, sl = slide)
    println("MANY2MANY finished")
    orthologyTable.writeOrthology(genes,  OrthologyMode.all , (folder / "all.tsv").pathAsString, sl = slide)
    println("ALL finished")
    println(s"writing orthology table for ${folder.pathAsString} FINISHED")
    println("-----------------------")
  }
}
