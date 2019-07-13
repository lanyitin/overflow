package tw.lanyitin.overflow

import java.io.{File, FileReader, FileWriter}

import io.circe.generic.auto._
import io.circe.yaml.parser
import org.apache.commons.cli.{CommandLine, CommandLineParser, DefaultParser, Options}
import org.slf4j.LoggerFactory
import tw.lanyitin.common.graph.Path


object Main {

  val ARGV_KEY_CONFIG = "config"

  val logger = LoggerFactory.getLogger(this.getClass)

  def main(args: Array[String]): Unit = {
    val config: Config = this.prepareCommandLine(args)

    this.logger.trace("prepare graph")

    val file = new File(config.model)
    val parser = DrawIOModalParser(config)
    val coverageCriterion = Option(config.criterion).getOrElse("all-path")
    val traversalFrontier = Option(config.traverse).getOrElse("bfs")

    val graphs = parser.parseGraph
    graphs.map(g => parser.transformation(g))

    .zip((1 to graphs.size).toList)

    .map(iteration => {
      val (graph, idx) = iteration
      val graphFolder = new File(file.getParentFile.getAbsolutePath, s"graph${idx}")
      if (!graphFolder.exists) {
        graphFolder.mkdir
      }
      val graphFile = new File(graphFolder, "graph.dot")
      this.logger.trace(s"graph file: ${graphFile.getAbsolutePath}")
      if (!graphFile.exists) {
        graphFile.createNewFile
      }
      val fileWriter = new FileWriter(graphFile)
      fileWriter.write(ElementInfoVisualizer.visualize(graph))
      fileWriter.close
      (graph, graphFolder, parser.loops(graph))
    })

    .foreach(iteration => {
      val (graph, graphFolder, loops) = iteration
      this.logger.trace("start path enumeration")

      val criterion = CoverageCriterionFactory.getCoverageCriterion(coverageCriterion, graph)
      val frontier: TraversalFrontier[Path[ElementInfo, ElementInfo]] = TraversalFrontierFactroy.getTraversalFrontier(traversalFrontier)
      val pathEnumerator: PathEnumerator[ElementInfo, ElementInfo] = new PathEnumerator[ElementInfo, ElementInfo](graph, loops, frontier, criterion)
      var count = 0
      while (!criterion.isMeetCriterion && frontier.length != 0) {
        println(s"\r is meet criterion ${criterion.isMeetCriterion}, frontier length: ${frontier.length}")
        val path = pathEnumerator.nextPath
        if (path != null) {
          this.logger.trace(path.toString)
          val pathFile = new File(graphFolder, s"path${count}.dot")
          count += 1
          val pathFileWriter = new FileWriter(pathFile)
          pathFileWriter.write(ElementInfoVisualizer.visualize(path))
          pathFileWriter.close
        }
      }
    })
  }

  def prepareCommandLine(args: Array[String]): Config = {
    // crate yaml parser
    // create Options object
    val options: Options = new Options()

    // add t option
    options.addOption("f", ARGV_KEY_CONFIG, true, "path of config");

    val cmdParser: CommandLineParser = new DefaultParser()

    val cmd: CommandLine = cmdParser.parse(options, args)

    val configFile: File = new File(cmd.getOptionValue(ARGV_KEY_CONFIG, "overflow.yml"))
    val parseResult = parser.parse(new FileReader(configFile)).map(_.as[Config])
    parseResult match {
      case Left(error) => throw error
      case Right(decodeResult) => decodeResult match {
        case Left(e) => throw e
        case Right(config) => config
      }
    }
  }
}

