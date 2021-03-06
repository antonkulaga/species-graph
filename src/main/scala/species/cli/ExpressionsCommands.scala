package species.cli

import _root_.enumeratum.{Enum, EnumEntry}
import com.monovore.decline.enumeratum._
import cats.implicits._
import com.monovore.decline._
import species.sparql.expressions.{ExpressionTable, MultiSpeciesExpressions, One2ManySettings}
import species.sparql.orthology.{OrthologyManager, OrthologyMode, OrthologyTable}
import better.files._
import species.sparql.samples.{EnsemblSpecies, SampleMini, Samples, Species}

import scala.collection.immutable._

sealed trait SplitExpressions extends EnumEntry with EnumEntry.Snakecase

object SplitExpressions extends Enum[SplitExpressions] {
  case object NoSplit extends SplitExpressions
  case object ByClass extends SplitExpressions
  case object ByClassAndTissue extends SplitExpressions
  case object ByTissueAndClass extends SplitExpressions
  case object ByTissue extends SplitExpressions

  val values = findValues
}

trait ExpressionsCommands  extends OrthologyCommands {

  protected def extract_OrthologyMode(setting: One2ManySettings, confidence: Confidence) = (setting, confidence) match {
    case (One2ManySettings.one2one_only, Confidence.high) => OrthologyMode.one2one_high
    case (One2ManySettings.one2one_only, confidence) => OrthologyMode.one2one
    case (One2ManySettings.sum_directed, _) => OrthologyMode.one2oneplus_directed
    case (other, Confidence.high) => OrthologyMode.default_high
    case (other, conf) => OrthologyMode.default
  }

  lazy val samples: Opts[String] = Opts.option[String](long = "samples", help = "samples (all by default))").withDefault("all")
  lazy val splitExpressions: Opts[SplitExpressions] = Opts.option[SplitExpressions](long = "split",
    help = "How to split expressions (nosplit, byclass, byclassandtissu, bytissue)").withDefault(SplitExpressions.NoSplit)
  lazy val one_2_many_settings: Opts[One2ManySettings] = Opts.option[One2ManySettings](long = "one2many",
    help = "How to handler one_2_many genes, can be: sum, sum_directed, separator, one2one_only, average  (one2one_only by default)").withDefault(One2ManySettings.one2one_only)


  /**
   * Function that implements expressions command
   * @param path File or Folder to write output
   * @param split how to split gene expressions
   * @param one2ManySettings
   * @param samples
   * @param genes
   * @param verbose
   * @param na
   * @param server
   * @param sep
   * @param rewrite
   * @param sl
   * @param confidence
   */
  def expressions_implementation(path: String, split:SplitExpressions,
                                 one2ManySettings: One2ManySettings,
                                 samples: String,
                                 genes: String,
                                 verbose: Boolean,
                                 na: String,
                                 server: String,
                                 sep: String,
                                 rewrite: Boolean,
                                 sl: Int,
                                 confidence: Confidence,
                                 with_prefix: Boolean

                                     ): Unit = time {

    (path: String, split:SplitExpressions,
      one2ManySettings: One2ManySettings,
      samples: String, genes: String, verbose, na, server: String, sep, rewrite, sl, confidence) match {

      case (path, split, one2ManySettings: One2ManySettings, samples, gs, gene_names, na, server, sep, rewrite, sl, confidence)
      =>
        val params = initialize_expressions(path, gs, samples, server)
        val exp = new MultiSpeciesExpressions(params.runs)
        val expressionTable: ExpressionTable = new ExpressionTable(params.referenceGenes, exp)
        if (rewrite) {
          if (params.folder.exists) {
            warn("output folder " + params.folder.pathAsString + " exists, deleting it")
            params.folder.delete()
          }
        }
        if (split != SplitExpressions.NoSplit) params.folder.createDirectoryIfNotExists(createParents = true)
        val mode: OrthologyMode = extract_OrthologyMode(one2ManySettings, confidence)
        split match {
          case SplitExpressions.ByTissue =>
            info(s"writing expressions in ${params.folder.pathAsString} splited by tissue")
            for {
              (category: String, table) <- expressionTable.splitByTissue()
              output = category.replace("ens:", "").replace(":", "") + ".tsv"
            }
              table.write_table((params.folder / output).pathAsString, mode, true ,sep,
                gene_names, na, one2ManySettings, sl, Int.MaxValue, !with_prefix, false)(params.orthologyManager)

          case SplitExpressions.ByClass =>
            info(s"writing expressions to ${params.folder.pathAsString} splited by animal class")
            for {
              (category: String, table) <- expressionTable.splitByClass()
              output = category.replace("ens:", "").replace(":", "") + ".tsv"
            }
              table.write_table((params.folder /  output).pathAsString,
                mode, true ,sep,
                gene_names, na, one2ManySettings, sl, Int.MaxValue, !with_prefix, false)(params.orthologyManager)

          case SplitExpressions.ByClassAndTissue =>
            info(s"writing expressions to ${params.folder.pathAsString} with subfolders for each animal class with files per each tissue")
            for {
              (category1, table1) <- expressionTable.splitByClass()
              (category2, table2) <- table1.splitByTissue()
              subfolder = category1.replace("ens:", "").replace(":", "")
              output = category2.replace("ens:", "").replace(":", "") + ".tsv"
            }
              table2.write_table((params.folder / subfolder / output).pathAsString,
                mode, true ,sep,
                gene_names, na, one2ManySettings, sl, Int.MaxValue, !with_prefix, false)(params.orthologyManager)

          case SplitExpressions.ByTissueAndClass =>
            info(s"writing expressions to ${params.folder.pathAsString} with subfolders for each tissue with files per each animal class")
            for {
              (category1: String, table1) <- expressionTable.splitByTissue()
              (category2: String, table2) <- table1.splitByClass()
              subfolder = category1.replace("ens:", "").replace(":", "")
              output = category2.replace("ens:", "").replace(":", "") + ".tsv"
            }
              table2.write_table((params.folder / subfolder / output).pathAsString,
                mode, true ,sep,
                gene_names, na, one2ManySettings, sl, Int.MaxValue, !with_prefix, false)(params.orthologyManager)

          case SplitExpressions.NoSplit =>
            info(s"writing all expressions to one file ${path}")
            expressionTable.write_table(path,mode, true ,sep,
              gene_names, na, one2ManySettings, sl, Int.MaxValue, !with_prefix, false)(params.orthologyManager)
        }
      }
    }


  /**
   * expresions command to generate Expression tables
   */
  lazy val expressions: Command[Unit] = Command(
    name = "expressions", header = "Generate expression tables"
  ) {
    (output, splitExpressions, one_2_many_settings, samples, genes, verbose, na, server, separator, rewrite, slide, confidence, prefixed).mapN {
      expressions_implementation
    }
  }

  /**
   * Functions that initialize gene expression parameters
   * @param path
   * @param gs
   * @param samples
   * @param server
   * @return
   */
  protected def initialize_expressions(path: String, gs: String, samples: String, server: String) = {
    implicit val orthologyManager: OrthologyManager = new OrthologyManager(server)
    val reference_genes = extract_genes(gs)
    val s = new Samples(server)
    val runs: Vector[SampleMini] = samples match {
      case "all" => s.samples_mini_by_runs()
      case cl if animal_classes.contains(cl) => s.samples_mini_by_runs().filter(s => s.tissue.contains(cl) || cl.contains(s.tissue))
      case other => s.samples_mini_by_runs(other.split(";").map(s.sra).toVector)
    }
    val species: Vector[EnsemblSpecies] = new Species(server).species_in_samples(runs.map(_.run))
    val folder = File(path)
    ExpressionParameters(folder, reference_genes.map(_.gene), runs, species, orthologyManager)
  }
}
/*
to avoid init boilerplate
 */
case class ExpressionParameters(  folder: File, referenceGenes: Vector[String], runs: Vector[SampleMini], species: Vector[EnsemblSpecies], orthologyManager: OrthologyManager
                               )