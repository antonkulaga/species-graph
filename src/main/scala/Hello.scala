import species.sparql._
import pprint.pprintln
import better.files._
import File._
import java.io.{File => JFile}
import species.tables._
import species.tables.{Aggregation, ExpressionTable, OrthologyTable}
import scala.collection.compat._

object Hello extends scala.App{

  def writeGenes(genes: Vector[String], species: Vector[EnsemblSpecies], folder: File) = {
    println(s"writing orthology table for ${folder.pathAsString}")
    folder.createDirectoryIfNotExists()
    val orthologyTable = new OrthologyTable(species)
    orthologyTable.writeOrthology(genes,  OrthologyMode.one2one, (folder / "one2one.tsv").pathAsString)
    println("ONE2ONE finished")
    orthologyTable.writeOrthology(genes,  OrthologyMode.one2many , (folder / "one2many.tsv").pathAsString)
    println("ONE2MANY finished")
    orthologyTable.writeOrthology(genes,  OrthologyMode.many2many,  (folder / "many2many.tsv").pathAsString)
    println("MANY2MANY finished")
    orthologyTable.writeOrthology(genes,  OrthologyMode.all , (folder / "all.tsv").pathAsString)
    println("ALL finished")
    println(s"writing orthology table for ${folder.pathAsString} FINISHED")
    println("-----------------------")
  }
  val g = Genes()
  val humanGenes: Vector[String] = g.speciesGenes()
  val species: Vector[EnsemblSpecies] = Species.get_species_in_samples()
  val classes = Vector(
    "http://rdf.ebi.ac.uk/resource/ensembl/Mammalia",
    "http://rdf.ebi.ac.uk/resource/ensembl/Aves",
    "http://rdf.ebi.ac.uk/resource/ensembl/Reptilia",
    "http://rdf.ebi.ac.uk/resource/ensembl/Teleostei",
    "http://rdf.ebi.ac.uk/resource/ensembl/Chondrichthyes",
    "http://rdf.ebi.ac.uk/resource/ensembl/Coelacanthi",
  )
  val species_by_class = classes.map(c=>species.filter(s=>s.animal_class == c)).reduce(_++_)
  writeGenes(humanGenes, species, folder = File("/data/species/by_class_and_lifespan"))

}
