import com.monovore.decline._
import species.cli.CLI

object Main  extends CommandApp(
  name = "species-graph",
  header = "Species GRAPH Client",
  main = {

    val mainCommand: Opts[Unit] =  Opts.subcommand(CLI.orthologs)
      .orElse(Opts.subcommand(CLI.expressions))
      .orElse(Opts.subcommand(CLI.genes_statistics))
      .orElse(Opts.subcommand(CLI.samples_index))
      .orElse(Opts.subcommand(CLI.species_index))
    mainCommand.map{ _=>

    }
  }
)