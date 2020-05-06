import species.sparql._
import pprint.pprintln
import better.files._
import File._
import java.io.{File => JFile}

import species.tables._
import species.tables.{Aggregation, ExpressionTable, OrthologyTable}

import scala.collection.compat._
import cats.implicits._
import java.nio
import java.nio.file.Path

import com.monovore.decline._
import species.cli.CLI

object Main  extends CommandApp(
  name = "species-graph",
  header = "Species GRAPH Client",
  main = {

    val mainCommand: Opts[Unit] =  Opts.subcommand(CLI.orthologs).orElse(Opts.subcommand(CLI.write_species))
    mainCommand.map{ _=>

    }
  }
)