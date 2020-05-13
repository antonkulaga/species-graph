package species.sparql.orthology

import scala.collection.immutable.ListMap

object Orthology{

  def self_orthology(reference_gene: String, symbol: String, species: String)  =
    Orthology(reference_gene, "ens:ortholog_one2one", reference_gene, symbol, species, "http://rdf.ebi.ac.uk/resource/ensembl/confidence/high" )
}
/**
 * Orthology relation for a gene
 * @param reference_gene
 * @param orthology
 * @param ortholog
 * @param ortholog_symbol
 * @param species
 * @param confidence
 */
case class Orthology(
                      reference_gene: String,
                      orthology: String,
                      ortholog: String,
                      ortholog_symbol: String,
                      species: String,
                      confidence: String,
                    )


/**
 * Settings to set up which ortholog genes we want to get
 */
object OrthologyMode{
  lazy val all = OrthologyMode(true, true, true, None)
  lazy val all_high = OrthologyMode(true, true, true, Some("high"))
  lazy val default = OrthologyMode(true, true, false, None)
  lazy val one2one = OrthologyMode(true, false, false, None)
  lazy val one2one_high = OrthologyMode(true, false, false, Some("high"))
  lazy val one2many = OrthologyMode(false, true, false, None)
  lazy val one2many_high = OrthologyMode(false, true, false, Some("high"))
  lazy val one2many_directed = OrthologyMode(false, true, false, None, true)
  lazy val one2many_high_directed = OrthologyMode(false, true, false, Some("high"), true)

  lazy val many2many = OrthologyMode(false, false, true, None)
  lazy val many2many_high = OrthologyMode(false, false, true, Some("high"))

}
case class OrthologyMode(  one2one: Boolean,
                           one2many: Boolean,
                           many2many: Boolean,
                           confidence: Option[String] = None, //can be high or low
                           directed: Boolean = false //for directed
                        )
{
 // require(one2one || one2many || many2many, "at least one type of orthology must be true!")
  private lazy val one2oneString: String = if(one2one) "ens:ortholog_one2one" else ""
  private lazy val one2manyString: String = if(one2many)
    if(directed) ":one2many" else "ens:ortholog_one2many" else ""
  private lazy val many2manyString: String = if(many2many) "ens:ortholog_many2many" else ""


  lazy val as_values = s"values ?orthology { ${one2oneString} ${one2manyString} ${many2manyString}  } . "
  lazy val with_confidence = confidence.fold("")(s=>s"VALUES ?confidence { <http://rdf.ebi.ac.uk/resource/ensembl/confidence/${s}> } .")
}
