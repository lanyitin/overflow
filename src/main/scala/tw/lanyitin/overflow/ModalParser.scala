package tw.lanyitin.overflow

import org.slf4j.LoggerFactory

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader
import java.net.URLDecoder;
import java.util.Base64
import java.util.zip.Inflater
import java.util.zip.Inflater;
import java.util.zip.InflaterInputStream;
import org.dom4j.Document
import org.dom4j.Element
import org.dom4j.io.SAXReader
import scala.util.Try

val reader = new SAXReader();

trait ModelParse[V, U] {
  def parse: Set[Graph[V, U]]
}


case class ElementInfo (val id: String, val value: String) {
  val strRegEx = "<[^>]*>";
  val content: String = Try(URLDecoder.decode(this.value).replaceAll(strRegEx, "").trim).getOrElse({
    if (value != null) {
      value
    } else {
      ""
    }
  })
  override def toString: String = content
}

sealed class DrawIOModalParse(val file: File) extends ModelParse[ElementInfo, ElementInfo] {
  // /home/lanyitin/Projects/Mega/doc/05-%E5%8A%9F%E8%83%BD%E9%9C%80%E6%B1%82%E8%A6%8F%E6%A0%BC%E6%9B%B8/20%20%E6%B5%81%E7%A8%8B%E5%9C%96/00%20%E7%99%BB%E5%87%BA%E5%85%A5%E9%A6%96%E9%A0%81/MB-%E5%BF%AB%E9%80%9F%E7%99%BB%E5%85%A5.xml
  val logger = LoggerFactory.getLogger(this.getClass)
  val document = reader.read(this.file)
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
