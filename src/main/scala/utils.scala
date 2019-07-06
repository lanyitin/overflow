package tw.lanyitin.overflow

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
import tw.lanyitin.common.graph.Graph
import tw.lanyitin.common.graph.GraphFactory
import tw.lanyitin.common.graph.Path
import tw.lanyitin.common.graph.Node
import tw.lanyitin.common.graph.Edge
import tw.lanyitin.common.graph.UndirectedEdge
import tw.lanyitin.common.graph.DirectedEdge

case class ElementInfo (val id: String, val value: String) {
  val strRegEx = "<[^>]*>";
  val content: String = Try(StringEscapeUtils.unescapeHtml4(URLDecoder.decode(this.value).replaceAll(strRegEx, "").trim)).getOrElse({
    if (value != null) {
      value
    } else {
      ""
    }
  })
  override def toString: String = s"(${id}) ${content}"
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
      case UndirectedEdge(n1, n2, data) => "%s -- %s [label = \"%s\"]\n".format(n1.payload.id, n2.payload.id, data.content)
      case DirectedEdge(n1, n2, data) => "%s -> %s [label = \"%s\"]\n".format(n1.payload.id, n2.payload.id, data.content)
    }
  }
  def visualize(node: Node[ElementInfo]): String = {
    "%s [label = \"%s\"]\n".format(node.payload.id, node.payload.content)
  }
}

trait DrawIOGraphFactory extends GraphFactory[ElementInfo, ElementInfo] {
  var pseudoId = 0;
  def pseudoNode: Node[ElementInfo] = {
    this.pseudoId += 1
    Node(ElementInfo("pseudoNode" + this.pseudoId, ""))
  }

  def pseudoEdge(node1: Node[ElementInfo], node2: Node[ElementInfo]): Edge[ElementInfo, ElementInfo] = {
    this.pseudoId += 1
    DirectedEdge(node1, node2, ElementInfo("pseudoEdge" + this.pseudoId, ""))
  }
}

case class DrawIOModalParser(val file: File, val isMCC: Boolean) extends ModelParser[ElementInfo, ElementInfo] with DrawIOGraphFactory {
  val reader = new SAXReader();
  // val logger = LoggerFactory.getLogger(this.getClass)
  val document = reader.read(this.file)

  def duplicateNode(node: Node[ElementInfo]) = {
    this.pseudoId += 1
    Node(ElementInfo("pseudoNode" + this.pseudoId, node.payload.content))
  }

  def isTruePath(edge: DirectedEdge[ElementInfo, ElementInfo]): Boolean = {
    edge.annotation != null && edge.annotation.value!= null && edge.annotation.value.equals("Y")
  }    
  def isFalsePath(edge: DirectedEdge[ElementInfo, ElementInfo]): Boolean = {
    edge.annotation != null && edge.annotation.value!= null && edge.annotation.value.equals("N")
  }

  def expr2Node(expr: Expression): Node[ElementInfo] = {
    this.pseudoId += 1
    Node(ElementInfo("expr" + this.pseudoId, this.exprToStr(expr)))
  }

  val exprWrapperPattern = "\\{\\{[^\\}]+\\}\\}".r

  def extractExpression(node: Node[ElementInfo]): String = 
    this.exprWrapperPattern.findAllIn(node.payload.toString).toList.map(seg => seg.replace("{{", "").replace("}}", "").replace("\n", "").trim).mkString(" ").trim

  def parseGraph: Set[Graph[ElementInfo, ElementInfo]] = {
    val iterator = document.selectNodes("/mxfile/diagram").iterator
    val decompresser = new Inflater()
    val decoder = Base64.getDecoder

    var diagramData: Set[String] = Set()
    while(iterator.hasNext) {
      val element: Element = iterator.next.asInstanceOf[Element]
      diagramData = diagramData + element.getStringValue
    }

    diagramData.map(data => {
      // this.logger.trace("diagram data: " + data)
      val result = decoder.decode(data)
      // this.logger.trace("result: " + result.length)
      result
      }).map((data: Array[Byte]) => {
        URLDecoder.decode(this.inflate(data), "UTF-8")
        }).map(inflated => {
          // this.logger.trace("inflated: " + inflated)
          reader.read(new StringReader(inflated))
          }).map(doc => {
            import collection.JavaConverters._
            import scala.language.implicitConversions
            val nodeElements: List[Element] = doc.selectNodes("/mxGraphModel/root/mxCell[@vertex=\"1\"]").asInstanceOf[java.util.List[Element]].asScala
              .filter(node => node.attributeValue("connectable") == null || !node.attributeValue("connectable").equals("0"))
              .toList
              val edgeElements: List[Element] = doc.selectNodes("/mxGraphModel/root/mxCell[@edge=\"1\"]").asInstanceOf[java.util.List[Element]].asScala.toList
              val noEndEdge: List[Element] = edgeElements.filter(elem => elem.attributeValue("source") == null || elem.attributeValue("target") == null)
              if (noEndEdge.size > 0) {
                val represent = noEndEdge.map(edge => {
                    if (edge.attributeValue("source") != null) {
                      nodeElements
                        .filter(node => node.attributeValue("id") == edge.attributeValue("source"))
                        .map(node => node.attributeValue("value") + "-> <none>")
                    } else {
                      nodeElements
                        .filter(node => node.attributeValue("id") == edge.attributeValue("target"))
                        .map(node => "<none> -> " + node.attributeValue("value"))
                    }
                  })
                  throw new Error(represent.flatten.mkString("\n"))
              }
              this.logger.trace("nodes in model: " + nodeElements.length)
              this.logger.trace("edges in model: " + edgeElements.length)

              val nodes: Set[Node[ElementInfo]] = nodeElements.map((elem: Element) => Node(ElementInfo(elem.attributeValue("id").replaceAll("_", "").replaceAll("-", ""), elem.attributeValue("value")))).toSet
              val edges: Set[Edge[ElementInfo, ElementInfo]] = edgeElements.toSet.map((elem: Element) => {
                val source = nodes.find(node => node.payload.id == elem.attributeValue("source").replaceAll("_", "").replaceAll("-", ""))
                val target = nodes.find(node => node.payload.id == elem.attributeValue("target").replaceAll("_", "").replaceAll("-", ""))
                val result = for {
                  s <- source
                  t <- target
                } yield Set(DirectedEdge(s, t, ElementInfo(elem.attributeValue("id").replaceAll("_", "").replaceAll("-", ""), elem.attributeValue("value"))))

                if (result.isEmpty) {
                  Set()
                } else {
                  result.get
                }
              }).flatten

            this.logger.trace("nodes parsed: " + nodes.size)
            this.logger.trace("edges parsed: " + edges.size)

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
      // this.logger.trace("char: " + ch.asInstanceOf[Char])
      buffer.append(ch.asInstanceOf[Char]);
      ch = in.read()
    }

    in.close();

    return buffer.toString();
  }
}