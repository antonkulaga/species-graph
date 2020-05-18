package species.cli

import com.monovore.decline.Opts
case object CLI extends CLI
class CLI extends OrthologyCommands with ExpressionsCommands with StatistictsCommands {
  lazy val mainCommand: Opts[Unit] =  Opts.subcommand(CLI.orthologs)
    .orElse(Opts.subcommand(CLI.expressions))
    .orElse(Opts.subcommand(CLI.genes_statistics))
    .orElse(Opts.subcommand(CLI.samples_index))
    .orElse(Opts.subcommand(CLI.species_index))

}
