import java.io.BufferedReader
import java.io.ByteArrayInputStream
import java.io.File
import java.io.File
import java.io.FileWriter
import java.io.IOException
import java.io.InputStreamReader
import java.io.Reader
import java.io.StringReader
import java.net.URLDecoder
import java.util.Base64
import java.util.zip.Inflater
import java.util.zip.Inflater
import java.util.zip.InflaterInputStream
import org.apache.commons.cli.CommandLine
import org.apache.commons.cli.CommandLineParser
import org.apache.commons.cli.HelpFormatter
import org.apache.commons.cli.Options
import org.apache.commons.cli.PosixParser
import org.apache.commons.lang3.StringEscapeUtils
import org.dom4j.Document
import org.dom4j.Element
import org.dom4j.Element
import org.dom4j.io.SAXReader
import org.slf4j.LoggerFactory
import scala.util.Try
import tw.lanyitin.common.ast.Expression
import tw.lanyitin.huevo.parse.Parser
import tw.lanyitin.overflow.Visualizer
import tw.lanyitin.common.graph.Graph
import tw.lanyitin.common.graph.GraphFactory
import tw.lanyitin.common.graph.Path
import tw.lanyitin.common.graph.Node
import tw.lanyitin.common.graph.Edge
import tw.lanyitin.common.graph.UndirectedEdge
import tw.lanyitin.common.graph.DirectedEdge
import tw.lanyitin.overflow.AllEdgeCriterion
import tw.lanyitin.overflow.TraversalFrontier
import tw.lanyitin.overflow.QueueFrontier
import tw.lanyitin.overflow.PathEnumerator
import tw.lanyitin.overflow.ModelParser
import tw.lanyitin.overflow.DrawIOModalParser
import tw.lanyitin.overflow.ElementInfo
import tw.lanyitin.overflow.CoverageCriterionFactory
import tw.lanyitin.overflow.TraversalFrontierFactroy
import tw.lanyitin.overflow.ElementInfoVisualizer

object Main {
  val ARGV_KEY_MODEL = "model"
  val ARGV_KEY_COVERAGE_CRITERION = "criterion"
  val ARGV_KEY_TRAVERSE = "traverse"
  val ARGV_KEY_MCC = "mcc"

  val logger = LoggerFactory.getLogger(this.getClass)

  def main(args: Array[String]): Unit = {
    val cmd: Option[CommandLine] = this.prepareCommandLine(args);

    cmd match {
      case Some(argv) => {
        this.logger.trace("prepare graph")

        val file = new File(argv.getOptionValue(ARGV_KEY_MODEL))
        val parser = DrawIOModalParser(file, argv.hasOption(ARGV_KEY_MCC))
        val coverageCriterion = argv.getOptionValue(ARGV_KEY_COVERAGE_CRITERION, "all-edge")
        val traversalFrontier = argv.getOptionValue(ARGV_KEY_TRAVERSE, "bfs")

        val graphs = parser.parseGraph
        graphs.map(g => parser.transformation(g)).zip((1 to graphs.size).toList)
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
          (graph, idx, graphFolder)
        }).foreach(iteration => {
          val (graph, idx, graphFolder) = iteration
          this.logger.trace("start path enumation");

          val criterion = CoverageCriterionFactory.getCoverageCriterion(coverageCriterion, graph)
          val frontier: TraversalFrontier[Path[ElementInfo, ElementInfo]] = TraversalFrontierFactroy.getTraversalFrontier(traversalFrontier)
          val pathEnumerator: PathEnumerator[ElementInfo, ElementInfo] = new PathEnumerator[ElementInfo, ElementInfo](graph, frontier, criterion)
          var count = 0;
          while (!criterion.isMeetCriterion && frontier.length != 0) {
            // this.logger.trace(s"unvisited edges: ${graph.edges -- criterion.visitedEdges}")
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
      case None => println
    }
  }



  def prepareCommandLine(args: Array[String]): Option[CommandLine] = {
    // create Options object
    val options: Options = new Options()

    // add t option
    options.addOption("m", ARGV_KEY_MODEL, true, "path of model file");
    options.addOption(null, ARGV_KEY_MCC, false, "enable multiple condition coverage")
    options.addOption(null, ARGV_KEY_COVERAGE_CRITERION, true, "coverage criterion")
    options.addOption(null, ARGV_KEY_TRAVERSE, true, "bread first traversal or depth first traversal")

    val parser: CommandLineParser = new PosixParser();

    val cmd: CommandLine = parser.parse(options, args)

    if (!cmd.hasOption(ARGV_KEY_MODEL)) {
      val formatter: HelpFormatter = new HelpFormatter();
      formatter.printHelp( "overflow", options );
      println(cmd)
      None
    } else {
      Some(cmd)
    }
  }
}
