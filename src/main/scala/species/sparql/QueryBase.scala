package species.sparql

import org.eclipse.rdf4j.repository._
import org.eclipse.rdf4j.repository.manager.RemoteRepositoryManager
import org.eclipse.rdf4j.sparqlbuilder.constraint.Operand
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.{GraphName, GraphPatterns, TriplePattern}
import org.eclipse.rdf4j.sparqlbuilder.rdf.{RdfObject, RdfPredicate, RdfSubject}

import scala.collection.immutable.ListMap
import scala.util.{Failure, Success, Try}

/**
 * Trait that does queries to graphdb
 */
trait QueryBase extends Prefixes {

  val has_gene = u(":has_gene")

  val orthology_one2one = ens("ortholog_one2one")
  val orthology_one2many = ens("ortholog_one2many")
  val orthology_many2many = ens("ortholog_many2many")

  def select_query(query: String): Vector[ListMap[String, String]] = withConnection{ con =>
    Try{
      con.prepareTupleQuery(query).evaluate().map(f=>f.toMapString).toVector
    } match {
      case Success(value) => value
      case Failure(e) =>
        println("FAILED SELECT QUERY:")
        println(query)
        throw e
    }
  }

  def update_and_check_query(update: String, query: String): Try[Vector[ListMap[String, String]]] = Try{
    insert_query(update)
    select_query(query)
  }

  def insert_query(UpdateQuery: String) = withConnection{ con =>
    con.prepareUpdate(UpdateQuery).execute()
  }

  def serverURL: String //="http://10.40.3.21:7200/"
  lazy val repositoryManager = {
    val manager = new RemoteRepositoryManager( serverURL )
    manager.init()
    manager
  }
  lazy val repository = repositoryManager.getRepository("ensembl")


  def withConnection[R](fun: RepositoryConnection => R): R = {
    val con: RepositoryConnection = repository.getConnection()
    con.setNamespace(":", "http://aging-research.group/resource/")
    con.setNamespace("ens", "http://rdf.ebi.ac.uk/resource/ensembl/")
    con.setNamespace("samples", "http://aging-research.group/samples/")
    val result = fun(con)
    con.close()
    result
  }

 object RDF{
   def variable(str: String) = {
     SparqlBuilder.`var`(str)
   }

   def res(str: String): RdfSubject
     with RdfPredicate
     with RdfObject
     with Operand
     with GraphName =
     if(str.startsWith("?")) variable(str.tail) else
       iri(u(str))


   def triple(subject: String, predicate: String, obj: String): TriplePattern = {
     GraphPatterns.tp(res(subject), res(predicate), res(obj))
   }
 }
  def values(name: String, value: String): String = if(value == "") "" else s"VALUES ?${name.replace("?", "")} { ${value} } ."
  def values(name: String, vals: Seq[String]): String = if(vals.isEmpty) "" else values(name, vals.mkString(" "))


}
