package species

import org.eclipse.rdf4j.model.Value
import org.eclipse.rdf4j.query.{Binding, BindingSet, TupleQueryResult}
import scala.collection.immutable._
//import scala.collection.JavaConverters._
import scala.jdk.CollectionConverters._
import scala.collection.compat._

package object sparql {
  implicit class QueryIterable(it: TupleQueryResult) extends Iterable[BindingSet]{
    override def iterator: Iterator[BindingSet] = it.iterator().asScala
  }

  implicit class BindingSetExt(bs: BindingSet) {
    def toMapRdf: ListMap[String, Value] = {
      ListMap.from(bs.getBindingNames.asScala.map(b=>bs.getBinding(b).toRdfTuple))
    }

    def toMapString: ListMap[String, String] = {
      ListMap.from(bs.getBindingNames.asScala.collect{case b if bs.hasBinding(b)=>bs.getBinding(b).toStringTuple})
    }
  }
  implicit class BindingExt(binding: Binding) {
    def toRdfTuple: (String, Value) = {
      binding.getName -> binding.getValue
    }

    def toStringTuple: (String, String) = {
      binding.getName -> binding.getValue.stringValue()
    }
  }

}
