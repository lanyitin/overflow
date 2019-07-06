package tw.lanyitin.overflow

import org.slf4j.LoggerFactory
import tw.lanyitin.common.graph.Graph
import tw.lanyitin.common.graph.Path
import tw.lanyitin.common.graph.DirectedEdge
import tw.lanyitin.common.graph.UndirectedEdge
import tw.lanyitin.common.graph.Node

sealed trait CoverageCriterion[V, U] {
  def graph: Graph[V,U]
  def addVisitPath(path: Path[V, U]): Unit
  def isMeetCriterion: Boolean
  def isVisited(path: Path[V, U]): Boolean
}

case class AllNodeCriterion[V, U](val graph: Graph[V,U]) extends CoverageCriterion[V, U] {
  val logger = LoggerFactory.getLogger(this.getClass)
  var visitedNodes: Set[Node[V]] = Set();
  var visitedPath: Set[Path[V, U]] = Set();
  def addVisitPath(path: Path[V, U]): Unit = {
    this.visitedPath = this.visitedPath + path
    this.visitedNodes = this.visitedNodes | path.edges.flatMap(edge => Set(edge.from, edge.to)).toSet
  }

  def isMeetCriterion: Boolean = {
    this.logger.trace("visited nodes: " + this.visitedNodes)
    this.logger.trace("target nodes: " + graph.nodes)
    this.visitedNodes.equals(graph.nodes)
  }

  def isVisited(path: Path[V, U]): Boolean = {
    this.visitedPath.contains(path)
  }
}

case class AllEdgeCriterion[V, U](val graph: Graph[V,U]) extends CoverageCriterion[V, U] {
  val logger = LoggerFactory.getLogger(this.getClass)
  var visitedEdges: Set[DirectedEdge[V, U]] = Set()
  var visitedPath: Set[Path[V, U]] = Set();
  val targetEdges = graph.directedEdges | graph.undirectedEdges.flatMap(edge => {
    Set(DirectedEdge(edge.node1, edge.node2, edge.annotation), DirectedEdge(edge.node2, edge.node1, edge.annotation))
  })
  def addVisitPath(path: Path[V, U]): Unit = {
    this.visitedPath = this.visitedPath + path
    this.visitedEdges = this.visitedEdges | path.edges.toSet
  }

  def isMeetCriterion: Boolean = {
    this.logger.trace("visited edges: " + this.visitedEdges)
    this.logger.trace("target edges: " + this.targetEdges)
    this.logger.trace("missing edge: ", this.targetEdges -- this.visitedEdges)
    this.visitedEdges.equals(this.targetEdges)
  }

  def isVisited(path: Path[V, U]): Boolean = {
    this.visitedPath.contains(path)
  }
}

case class AllPathCriterion[V, U](val graph: Graph[V,U]) extends CoverageCriterion[V, U] {
  val logger = LoggerFactory.getLogger(this.getClass)
  var visitedPath: Set[Path[V, U]] = Set();

  def addVisitPath(path: Path[V, U]): Unit = {
    this.visitedPath = this.visitedPath + path
  }

  def isMeetCriterion: Boolean = {
    false
  }

  def isVisited(path: Path[V, U]): Boolean = {
    this.visitedPath.contains(path)
  }
}

object CoverageCriterionFactory {
  def getCoverageCriterion[V, U](criterion: String, graph: Graph[V, U]): CoverageCriterion[V, U] = criterion match {
    case "all-edge" => AllEdgeCriterion(graph)
    case "all-node" => AllNodeCriterion(graph)
    case "all-path" => AllPathCriterion(graph)
  }
}