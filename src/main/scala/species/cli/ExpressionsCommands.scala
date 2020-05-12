package species.cli

import _root_.enumeratum.{Enum, EnumEntry}
import com.monovore.decline.enumeratum._
import cats.implicits._
import com.monovore.decline._
import species.sparql.expressions.{ExpressionTable, MultiSpeciesExpressions, SampleMini, Samples}
import species.sparql.orthology.{EnsemblSpecies, OrthologyManager, OrthologyMode, OrthologyTable, Species}
import better.files._

import scala.collection.immutable._

sealed trait SplitExpressions extends EnumEntry with EnumEntry.Lowercase

object SplitExpressions extends Enum[SplitExpressions] {
  case object NoSplit extends SplitExpressions
  case object ByClass extends SplitExpressions
  case object ByClassAndTissue extends SplitExpressions
  case object ByTissue extends SplitExpressions

  val values = findValues
}

sealed trait One2ManySettings extends EnumEntry with EnumEntry.Lowercase
object One2ManySettings extends Enum[One2ManySettings]{
  case object one2one_only extends One2ManySettings
  case object separator extends One2ManySettings
  case object sum extends One2ManySettings
  case object average extends One2ManySettings

  val values = findValues
}
/*
sealed trait OrthologySettings extends EnumEntry with EnumEntry.Lowercase
object OrthologySettings extends Enum[OrthologySettings]
{

  case object  all extends OrthologySettings// = OrthologyMode(true, true, true, None)
  case object  all_high extends OrthologySettings // = OrthologyMode(true, true, true, Some("high"))
  case object  default extends OrthologySettings // = OrthologyMode(true, true, false, None)
  case object  one2one extends OrthologySettings // = OrthologyMode(true, false, false, None)
  case object  one2one_high extends OrthologySettings // = OrthologyMode(true, false, false, Some("high"))
  case object  one2many extends OrthologySettings // = OrthologyMode(false, true, false, None)
  case object  one2many_high extends OrthologySettings // = OrthologyMode(false, true, false, Some("high"))
  case object  one2many_directed extends OrthologySettings // = OrthologyMode(false, true, false, None, true)
  case object  one2many_high_directed extends OrthologySettings // = OrthologyMode(false, true, false, Some("high"), true)

  case object  many2many extends OrthologySettings // = OrthologyMode(false, false, true, None)
  case object  many2many_high extends OrthologySettings // = OrthologyMode(false, false, true, Some("high"))

  val values = findValues
}
*/

trait ExpressionsCommands  extends OrthologyCommands {

  protected def extract_OrthologyMode(setting: One2ManySettings) = setting match {
    case One2ManySettings.one2one_only => OrthologyMode.one2one
    case other => OrthologyMode.default
  }

  lazy val samples: Opts[String] = Opts.option[String](long = "samples", help = "samples (all by default))").withDefault("all")
  lazy val expressionsPath: Opts[String] = Opts.option[String](long = "path", help = "Folder to store orthology tables")
  lazy val splitExpressions: Opts[SplitExpressions] = Opts.option[SplitExpressions](long = "split",
    help = "How to split expressions (nosplit, byclass, byclassandtissu, bytissue)").withDefault(SplitExpressions.NoSplit)
  lazy val one_2_many_settings: Opts[One2ManySettings] = Opts.option[One2ManySettings](long = "one_2_many_settings",
    help = "How to handler one_2_many genes (process one2one only by default").withDefault(One2ManySettings.one2one_only)


  def write_expression_implementation(path: String, split:SplitExpressions,
                                      one2ManySettings: One2ManySettings,
                                      samples: String, genes: String,  server: String): Unit =
    (path: String, split:SplitExpressions,
      one2ManySettings: One2ManySettings,
      samples: String, genes: String,  server: String) match {

      case (path, SplitExpressions.NoSplit, one2ManySettings: One2ManySettings, samples, gs,  server) =>
        val params = initialize_expressions(path, gs, samples, server)
        val exp = new MultiSpeciesExpressions(params.runs)
        val expressionTable: ExpressionTable = new ExpressionTable(params.referenceGenes, exp)
        //val expressions = exp.get_expressions_in_samples(runs)
        val mode = extract_OrthologyMode(one2ManySettings)
        expressionTable.write_table(path, mode)(params.orthologyManager)


      case (path, SplitExpressions.ByClass, one2ManySettings: One2ManySettings, samples, gs,  server) =>
        val params = initialize_expressions(path, gs, samples, server)
        val exp = new MultiSpeciesExpressions(params.runs)
        val expressionTable: ExpressionTable = new ExpressionTable(params.referenceGenes, exp)
        params.folder.createDirectoryIfNotExists()
        val mode = extract_OrthologyMode(one2ManySettings)
        for{
          (animal_class: String, table) <- expressionTable.splitByClass()
        }
        {
          table.write_table((params.folder / animal_class).pathAsString, mode)(params.orthologyManager)
        }
      case _ => println("OTHER")
  }


  lazy val expressions: Command[Unit] = Command(
    name = "expressions", header = "Generate expression tables"
  ) {

    (expressionsPath, splitExpressions, one_2_many_settings, samples, genes, server).mapN {
      write_expression_implementation
    }
  }
  protected def initialize_expressions(path: String, gs: String, samples: String, server: String) = {
    implicit val orthologyManager = new OrthologyManager(server)
    val reference_genes = extract_genes(gs)
    val s = new Samples(server)

    val runs: Vector[SampleMini] = samples match {
      case "all" => s.samples_mini_by_runs()
      case cl if animal_classes.contains(cl) => s.samples_mini_by_runs().filter(s => s.tissue.contains(cl) || cl.contains(s.tissue))
      case other => s.samples_mini_by_runs(other.split(";").toVector)
    }
    val species: Vector[EnsemblSpecies] = new Species(server).species_in_samples(runs.map(_.run))
    val folder = File(path)
    ExpressionParameters(folder, reference_genes, runs, species, orthologyManager)
  }
}
/*
to avoid init boilerplate
 */
case class ExpressionParameters(  folder: File, referenceGenes: Vector[String],
                               runs: Vector[SampleMini],
                               species: Vector[EnsemblSpecies],
                                  orthologyManager: OrthologyManager
                               )