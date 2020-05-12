package species.sparql

import org.scalatest.wordspec._
import species.sparql.orthology.{OrthologyManager, OrthologyMode}

import scala.collection.immutable._

class OrthilogySpec extends AnyWordSpec {

  protected def cut(str: String) = str.substring(Math.max(str.lastIndexOf("/"),str.lastIndexOf(":"))+1)

  "orthology" should   {
    "get proper genes" in {

      val ground_genes = Set(
        "ens:ENSMUSG00000020053","ens:ENSSSCG00000000857","ens:ENSBTAG00000011082"
      )

      val server = "http://10.40.3.21:7200/"
      val ortho = new OrthologyManager(server)
      val simpleOrthologs = ortho.orthologs(Vector("ens:ENSG00000017427"), OrthologyMode.all,Seq(":Mus_musculus", ":Bos_taurus", ":Sus_scrofa"))
      assert(simpleOrthologs.size == 3)
      assert(simpleOrthologs.map(o=>cut(o.ortholog)).toSet === ground_genes.map(cut))

      val by_ref = ortho.orthologs_by_ref(Vector("ens:ENSG00000017427"), OrthologyMode.all,Seq(":Mus_musculus", ":Bos_taurus", ":Sus_scrofa"))
      assert(by_ref.keySet.map(cut) === Set(cut("ens:ENSG00000017427")))

      val query = """
        |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        |PREFIX owl: <http://www.w3.org/2002/07/owl#>
        |PREFIX ens: <http://rdf.ebi.ac.uk/resource/ensembl/>
        |PREFIX samples:<http://aging-research.group/samples/>
        |PREFIX : <http://aging-research.group/resource/>
        |
        |SELECT * WHERE
        |{
        |    values ?target_species {:Mus_musculus :Bos_taurus :Sus_scrofa}    #put species selected by the user (info from selected samples)
        |    values ?reference_gene { ens:ENSG00000017427 } . #put reference genes selected by the user
        |    GRAPH ?confidence {
        |      values ?orthology { ens:ortholog_one2one ens:ortholog_one2many ens:ortholog_many2many } .
        |      ?reference_gene ?orthology ?ortholog .
        |	}
        |    ?target_species :has_gene ?ortholog .
        |    ?ortholog rdfs:label ?ortholog_symbol
        |}
        |""".stripMargin

      //val results2 = ortho.select_query(query)
    }
  }
}
/*
class SetSpec extends WordSpec {
  // Describe a scope for a subject, in this case: "A Set"
  "A Set" can { // All tests within these curly braces are about "A Set"

    // Can describe nested scopes that "narrow" its outer scopes
    "empty" should { // All tests within these curly braces are about "A Set (when empty)"

      "have size 0" in {    // Here, 'it' refers to "A Set (when empty)". The full name
        assert(Set.empty.size == 0) // of this test is: "A Set (when empty) should have size 0"
      }
      "produce NoSuchElementException when head is invoked" in { // Define another test
        intercept[NoSuchElementException] {
          Set.empty.head
        }
      }
      "should be empty" ignore { // To ignore a test, change 'it' to 'ignore'...
        assert(Set.empty.isEmpty)
      }
    }

    // Describe a second nested scope that narrows "A Set" in a different way
    "non-empty" should { // All tests within these curly braces are about "A Set (when non-empty)"

      "have the correct size" in { // Here, 'it' refers to "A Set (when non-empty)". This test's full
        assert(Set(1, 2, 3).size == 3)     // name is: "A Set (when non-empty) should have the correct size"
      }
      // Define a pending test by using (pending) for the body
      "return a contained value when head is invoked" is (pending)
      import tagobjects.Slow
      "be non-empty" taggedAs (Slow) in { // Tag a test by placing a tag object after the test name
        assert(Set(1, 2, 3).nonEmpty)
      }
    }
  }
}*/