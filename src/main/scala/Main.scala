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