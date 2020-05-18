import com.monovore.decline._
import species.cli.CLI

object Main  extends CommandApp(
  name = "species-graph",
  header = "Species GRAPH Client",
  main = {
    CLI.mainCommand.map{ _ =>

    }
  }
)