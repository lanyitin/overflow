import java.io.BufferedReader
import java.io.ByteArrayInputStream
import java.io.File
import java.io.File
import java.io.FileWriter
import java.io.IOException
import java.io.InputStreamReader
import java.io.Reader;
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
import tw.lanyitin.huevo.parse.Expression
import tw.lanyitin.huevo.parse.Parser
import tw.lanyitin.overflow.Visualizer
import tw.lanyitin.overflow._



object Main {

  val logger = LoggerFactory.getLogger(this.getClass)

  def main(args: Array[String]): Unit = {
    val cmd: Option[CommandLine] = this.prepareCommandLine(args);

    cmd match {
      case Some(argv) => {
        this.logger.trace("prepare graph")
        // var file = new File("/home/lanyitin/Projects/Mega/doc/05-功能需求規格書/20 流程圖/00 登出入首頁/MB-快速登入.xml")
        val file = new File(argv.getOptionValue("model"))
        val parser = DrawIOModalParser(file)
        parser.parse.map(g => parser.transformation(g))
          .foreach(graph => {
            val graphFile = new File(file.getParentFile.getAbsolutePath, "graph.dot")
            this.logger.debug(s"graph file: ${graphFile.getAbsolutePath}")
            if (!graphFile.exists) {
              graphFile.createNewFile
            }
            val fileWriter = new FileWriter(graphFile)
            fileWriter.write(ElementInfoVisualizer.visualize(graph))

            this.logger.trace("start path enumation");

            val criterion = AllEdgeCriterion(graph)
            val frontier: TraversalFrontier[Path[ElementInfo, ElementInfo]] = new QueueFrontier()
            val pathEnumerator: PathEnumerator[ElementInfo, ElementInfo] = new PathEnumerator[ElementInfo, ElementInfo](graph, frontier, criterion)
            while (!criterion.isMeetCriterion && frontier.length != 0) {
              // this.logger.debug(s"unvisited edges: ${graph.edges -- criterion.visitedEdges}")
              val path = pathEnumerator.nextPath
              if (path != null) {
                this.logger.debug(path.toString)
                // fileWriter.write(ElementInfoVisualizer.visualize(path))/
              }
            }
            fileWriter.close
          })

      }
      case None => println
    }
  }



  def prepareCommandLine(args: Array[String]): Option[CommandLine] = {
    // create Options object
    val options: Options = new Options()

    // add t option
    options.addOption("m", "model", true, "path of model file");

    val parser: CommandLineParser = new PosixParser();

    val cmd: CommandLine = parser.parse(options, args)

    if (!cmd.hasOption("model")) {
      val formatter: HelpFormatter = new HelpFormatter();
      formatter.printHelp( "overflow", options );
      None
    } else {
      Some(cmd)
    }
  }
}

case class ElementInfo (val id: String, val value: String) {
  val strRegEx = "<[^>]*>";
  val content: String = Try(StringEscapeUtils.unescapeHtml4(URLDecoder.decode(this.value).replaceAll(strRegEx, "").trim)).getOrElse({
    if (value != null) {
      value
    } else {
      ""
    }
  })
  override def toString: String = content
}

case object ElementInfoVisualizer extends Visualizer[ElementInfo, ElementInfo, String, String, String, String] {
  var graphCount = 0
  var pathCount = 0

  def visualize(graph: Graph[ElementInfo, ElementInfo]): String = {
    val builder = new StringBuffer
    graphCount+=1
    builder.append(s"digraph g${graphCount}{\n")
    graph.nodes.foreach(node => builder.append(this.visualize(node)))
    graph.edges.foreach(edge => builder.append(this.visualize(edge)))
    builder.append("}\n")
    builder.toString
  }

  def visualize(path: Path[ElementInfo, ElementInfo]): String = {
    val builder = new StringBuffer
    pathCount+=1
    builder.append(s"digraph p${pathCount}{\n")
    path.edges.flatMap(edge => Set(edge.from, edge.to))
      .foreach(node => builder.append(this.visualize(node)))
      path.edges.foreach(edge => builder.append(this.visualize(edge)))
      builder.append("}\n")
      builder.toString
  }

  def visualize(edge: Edge[ElementInfo, ElementInfo]): String = {
    edge match {
      case UndirectedEdge(n1, n2, data) => "%s -- %s\n".format(n1.payload.id, n2.payload.id)
      case DirectedEdge(n1, n2, data) => "%s -> %s\n".format(n1.payload.id, n2.payload.id)
    }
  }
  def visualize(node: Node[ElementInfo]): String = {
    "%s [label = \"(%s)%s\"]\n".format(node.payload.id, node.payload.id, node.payload.content)
  }

}

sealed class DrawIOModalParser(val file: File) extends ModelParse[ElementInfo, ElementInfo] {
  // /home/lanyitin/Projects/Mega/doc/05-%E5%8A%9F%E8%83%BD%E9%9C%80%E6%B1%82%E8%A6%8F%E6%A0%BC%E6%9B%B8/20%20%E6%B5%81%E7%A8%8B%E5%9C%96/00%20%E7%99%BB%E5%87%BA%E5%85%A5%E9%A6%96%E9%A0%81/MB-%E5%BF%AB%E9%80%9F%E7%99%BB%E5%85%A5.xml
  val reader = new SAXReader();
  // val logger = LoggerFactory.getLogger(this.getClass)
  val document = reader.read(this.file)

  var pseudoId = 0;

  def isTruePath(edge: DirectedEdge[ElementInfo, ElementInfo]): Boolean = {
    edge.annotation != null && edge.annotation.value!= null && edge.annotation.value.equals("Y")
  }    
  def isFalsePath(edge: DirectedEdge[ElementInfo, ElementInfo]): Boolean = {
    edge.annotation != null && edge.annotation.value!= null && edge.annotation.value.equals("N")
  }

  def expr2Node(expr: Expression): Node[ElementInfo] = {
    this.pseudoId += 1
    Node(ElementInfo("pseudoNode" + this.pseudoId, this.exprToStr(expr)))
  }
  def pseudoNode: Node[ElementInfo] = {
    this.pseudoId += 1
    Node(ElementInfo("pseudoNode" + this.pseudoId, ""))
  }

  def pseudoEdge(node1: Node[ElementInfo], node2: Node[ElementInfo]): Edge[ElementInfo, ElementInfo] = {
    this.pseudoId += 1
    DirectedEdge(node1, node2, ElementInfo("pseudoEdge" + this.pseudoId, ""))
  }


  def parse: Set[Graph[ElementInfo, ElementInfo]] = {
    val iterator = document.selectNodes("/mxfile/diagram").iterator
    val decompresser = new Inflater()
    val decoder = Base64.getDecoder

    var diagramData: Set[String] = Set()
    while(iterator.hasNext) {
      val element: Element = iterator.next.asInstanceOf[Element]
      diagramData = diagramData + element.getStringValue
    }

    diagramData.map(data => {
      // this.logger.debug("diagram data: " + data)
      val result = decoder.decode(data)
      // this.logger.debug("result: " + result.length)
      result
      }).map((data: Array[Byte]) => {
        URLDecoder.decode(this.inflate(data), "UTF-8")
        }).map(inflated => {
          // this.logger.debug("inflated: " + inflated)
          reader.read(new StringReader(inflated))
          }).map(doc => {
            import collection.JavaConverters._
            import scala.language.implicitConversions
            val nodeElements: List[Element] = doc.selectNodes("/mxGraphModel/root/mxCell[@vertex=\"1\"]").asInstanceOf[java.util.List[Element]].asScala
              .filter(node => node.attributeValue("connectable") == null || !node.attributeValue("connectable").equals("0"))
              .toList
              val edgeElements: List[Element] = doc.selectNodes("/mxGraphModel/root/mxCell[@edge=\"1\"]").asInstanceOf[java.util.List[Element]].asScala.toList

              this.logger.debug("nodes in model: " + nodeElements.length)
              this.logger.debug("edges in model: " + edgeElements.length)

              val nodes: Set[Node[ElementInfo]] = nodeElements.map((elem: Element) => Node(ElementInfo(elem.attributeValue("id"), elem.attribute("value").getText))).toSet
              val edges: Set[Edge[ElementInfo, ElementInfo]] = edgeElements.toSet.flatMap(elem => {
                val source = nodes.find(node => node.payload.id == elem.attributeValue("source"))
                val target = nodes.find(node => node.payload.id == elem.attributeValue("target"))
                val result = for {
                  s <- source
                  t <- target
                } yield Set(DirectedEdge(s, t, ElementInfo(elem.attributeValue("id"), elem.attributeValue("value"))))

                if (result.isEmpty) {
                  Set()
                } else {
                  result.get
                }
              })

            this.logger.debug("nodes parsed: " + nodes.size)
            this.logger.debug("edges parsed: " + edges.size)

            Graph(nodes, edges)
          })
  }

  def inflate(binary: Array[Byte]): String = {
    val buffer = new StringBuffer()
    val in = new BufferedReader(new InputStreamReader(
      new InflaterInputStream(new ByteArrayInputStream(binary),
        new Inflater(true)), "ISO-8859-1"));
    var ch: Int = 0;
    ch = in.read()
    while (ch > -1)
    {
      // this.logger.debug("char: " + ch.asInstanceOf[Char])
      buffer.append(ch.asInstanceOf[Char]);
      ch = in.read()
    }

    in.close();

    return buffer.toString();
  }
}
