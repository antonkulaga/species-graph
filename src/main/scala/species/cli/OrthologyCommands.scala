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


sealed trait SplitGenes extends EnumEntry with EnumEntry.Snakecase

object SplitGenes extends Enum[SplitGenes] {
  case object NoSplit extends SplitGenes
  case object ByClass extends SplitGenes
  case object BySpecies extends SplitGenes

  val values = findValues
}


/**
 * Trait with CLI commands about writing orthology tables
 */
trait OrthologyCommands extends SamplesCommands {

  lazy val genes: Opts[String] = Opts.option[String](long = "genes", help = "reference genes: either list of exact genes or the species names to take all genes from (default :Homo_sapiens)").withDefault(":Homo_sapiens")

  lazy val confidence: Opts[Confidence] = Opts.option[Confidence](long = "confidence", help = "How confident are we in orthologs").withDefault(Confidence.all)

  lazy val split: Opts[SplitGenes] = Opts.option[SplitGenes](long = "split", help = "How to split files (no_split, by_class, by_species)").withDefault(SplitGenes.NoSplit)

  lazy val slide: Opts[Int] = Opts.option[Int](long = "slide", help = "splits genes into batches of <slide> genes (25000 by default)").withDefault(25000)

  lazy val verbose: Opts[Boolean] = Opts.flag(long = "verbose", "Includes additional information for debuging (i.e. gene id-s)").orFalse

  lazy val orthologs: Command[Unit] = Command(
    name = "orthologs", header = "Generate orthology tables"
  ) {
    (output, split, server, genes, slide, rewrite, na).mapN(orthologs_implementation)
  }

  /**
   * Extracts genes from command line parameters
   * @param gs
   * @param orthologyManager
   * @return
   */
  protected def extract_genes(gs: String)(implicit orthologyManager: OrthologyManager): Vector[String] = {
    if(gs.startsWith(":") && gs.contains("_")) {
      val result = orthologyManager.speciesGenes(gs)
      info(s"using all ${result.size} genes of ${gs}")
      result
    } else {
      if(gs.toLowerCase().contains("ens") || gs.contains(";") || gs.contains(",")) {
        val result = gs.split(delimiter(gs)).map(g=> orthologyManager.ens(g)).toVector
        info(result)
        result
      } else {
        val result = orthologyManager.speciesGenes((":" + gs).replace("::", ":"))
        info(s"using all ${result.size} genes of ${gs}")
        result
      }
    }

  }

  def orthologs_implementation(path: String, split:SplitGenes, server: String, genes: String, sl: Int, rewrite: Boolean, na: String) = time{
    (path, split, server, genes) match {

      case (path, SplitGenes.NoSplit, server, gs) =>
        val sp = new Species(server)
        val ref_sp: String = if(gs.startsWith(":") || (!gs.contains(":") && gs.contains("_"))) gs else {
          val ss = sp.species_for_genes(gs.split(delimiter(gs)).toVector)
          ss.headOption.map(_.latin_name).getOrElse(":Homo_sapiens")
        }
        val species: Vector[EnsemblSpecies] = sp.species_in_samples()
        implicit val orthologyManager = new OrthologyManager(server)
        val reference_genes = extract_genes(gs)
        val folder = File(path)
        writeGenes(reference_genes, (species.find(s=> s.latin_name.contains(ref_sp)).head+: species.filter(s=> !s.latin_name.contains(ref_sp))), folder, sl, rewrite, na)

    case (path, split, server, gs) if split == SplitGenes.ByClass | split == SplitGenes.BySpecies =>

      val folder = File(path)
      implicit val orthologyManager = new OrthologyManager(server)
      val reference_genes = extract_genes(gs)
      val speciesGrouped = new Species(server).species_in_samples().groupBy(s => split match {
        case SplitGenes.ByClass => s.animal_class.replace("ens:", "").replace(":", "")
        case SplitGenes.BySpecies => s.latin_name.replace("ens:", "").replace(":", "")
      })
      for {
        (cl, sp) <- speciesGrouped
        cl_name = cl
          .replace("http://rdf.ebi.ac.uk/resource/ensembl/", "")
          .replace("<", "").replace(">", "").replace("ens:", "")
      } {
        writeGenes(reference_genes, sp, folder / cl_name, sl, rewrite, na)
      }
    }

  }

   def writeGenes(genes: Vector[String], species: Vector[EnsemblSpecies], folder: File, slide: Int, rewrite: Boolean, na: String): Unit = {
    info(s"writing orthology table for ${folder.pathAsString}")
    if(rewrite && folder.exists){
      warn(s"${folder.pathAsString} already exists, deleting it to rewrite!")
    }
    folder.createDirectoryIfNotExists()
    val orthologyTable = new OrthologyTable(species)
    orthologyTable.writeOrthologs(genes,  OrthologyMode.one2one, (folder / "one2one.tsv").pathAsString, sl = slide, na = na)
    info("ONE2ONE finished")
    orthologyTable.writeOrthologs(genes,  OrthologyMode.one2many , (folder / "one2many.tsv").pathAsString, sl = slide, na = na)
    info("ONE2MANY finished")
    orthologyTable.writeOrthologs(genes,  OrthologyMode.one2many_directed , (folder / "one2many_directed.tsv").pathAsString, sl = slide, na = na)
    info("ONE2MANY directed finished")
    orthologyTable.writeOrthologs(genes,  OrthologyMode.many2many,  (folder / "many2many.tsv").pathAsString, sl = slide, na = na)
    info("MANY2MANY finished")
    orthologyTable.writeOrthologs(genes,  OrthologyMode.all , (folder / "all.tsv").pathAsString, sl = slide, na = na)
    info("ALL finished")
    info(s"writing orthology table for ${folder.pathAsString} FINISHED")
  }
}
