package species

import species.sparql.Orthology

import scala.collection.immutable.{Map, Vector}

package object tables {
  type Aggregation = Map[String, Vector[Orthology]]=>Map[String, String]
}
