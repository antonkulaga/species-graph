package species.cli

import _root_.enumeratum.{Enum, EnumEntry}
import com.monovore.decline.enumeratum._
import cats.implicits._
import com.monovore.decline._



sealed trait SplitExpressions extends EnumEntry with EnumEntry.Lowercase

object SplitExpressions extends Enum[SplitExpressions] {
  case object NoSplit extends SplitExpressions
  case object ByClass extends SplitExpressions
  case object ByClassAndTissue extends SplitExpressions
  case object ByTissue extends SplitExpressions

  val values = findValues
}

trait ExpressionsCommands  extends OrthologyCommands {
  lazy val samples: Opts[String] = Opts.option[String](long = "samples", help = "samples (all by default))").withDefault("all")

  lazy val expressionsPath: Opts[String] = Opts.option[String](long = "path", help = "Folder to store orthology tables")
  lazy val splitExpressions: Opts[SplitExpressions] = Opts.option[SplitExpressions](long = "split",
    help = "How to split expressions (nosplit, byclass, byclassandtissu, bytissue)").withDefault(SplitExpressions.NoSplit)

  lazy val expressions: Command[Unit] = Command(
    name = "expressions", header = "Generate expression tables"
  ) {

    (expressionsPath, splitExpressions, samples, server).mapN{
      /*
    case (path, SplitExpressions.NoSplit, samples, server) =>
      val s = new Samples()
      val runs = samples match {
        case "all" => s.get_all_samples_mini()
        case cl if animal_classes.contains(cl) =>s.get_all_samples_mini().filter(s=>s.tissue.contains(cl) || cl.contains(s.tissue))
        case other => s.get_all_samples_mini_by_runs(other.split(";").toVector)
      }
      val species: Vector[EnsemblSpecies] = new Species(server).get_species_in_samples()
      val folder = File(path)
      val e = new GeneExpressions(server)
      val expressions = e.get_expressions_in_samples(runs)

      //writeGenes(humanGenes, species, folder)


    case (path, SplitExpressions.ByClass, server) =>
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

    case (path, cl, server) =>
      val classes: Array[String] = if(cl.contains(";")) cl.split(";") else cl.split(",")
      val folder = File(path)
      val speciesGrouped= new Species(server).get_species_in_samples().groupBy(_.animal_class).filterKeys(k=>classes.exists(c=>k.contains(c)))
      for{
        (cl, sp) <- speciesGrouped
        cl_name = cl
          .replace("http://rdf.ebi.ac.uk/resource/ensembl/", "")
          .replace("<", "").replace(">", "").replace("ens:", "")
      }{
        writeGenes(humanGenes, sp, folder / cl_name)
      }
      println("OTHER")

         */
      case _ => println("OTHER")
    }
  }

}
