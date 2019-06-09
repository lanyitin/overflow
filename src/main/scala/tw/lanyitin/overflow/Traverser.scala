
package tw.lanyitin.overflow

import scala.collection.mutable.Stack
import scala.collection.mutable.Queue
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

trait TraversalFrontier[V] {
  def add(node: V): Unit
  def pop: V
  def length: Integer
  def contains(node: V): Boolean
}
case class StackFrontier[V] extends TraversalFrontier[V] {
  val stack: Stack[V] = Stack()
  def add(node: V): Unit = {
    this.stack.push(node)
  }
  def pop: V = {
    this.stack.pop
  }
  def contains(node: V): Boolean = {
    this.stack.contains(node)
  }

  def length = this.stack.length
}
case class QueueFrontier[V] extends TraversalFrontier[V] {
  val queue: Queue[V] = Queue()

  def add(node: V): Unit = {
    this.queue.enqueue(node)
  }
  def pop: V = {
    this.queue.dequeue
  }
  def contains(node: V): Boolean = {
    this.queue.contains(node)
  }

  def length = this.queue.length
}

class PathEnumerator[V, U](val graph: Graph[V, U], val frontier: TraversalFrontier[Path[V, U]], val critorion: CoverageCriterion[V, U]) {
  val logger: Logger = LoggerFactory.getLogger(this.getClass)
  val beginNodes = this.graph.beginNodes
  val endNodes = this.graph.endNodes

  this.logger.trace("number of begin nodes: " + beginNodes.size)
  this.logger.trace(s"begin nodes: ${this.beginNodes}")
  this.beginNodes.foreach(node1 => {
    val outgoingEdges = graph.outgoingEdges(node1)
    outgoingEdges.foreach(edge => {
      var candicate: Path[V, U] = Path(List(edge))
      this.frontier.add(candicate)
    })
  })

  def nextPath: Path[V,U] = {
    var result: Path[V,U] = null
    this.logger.trace("frontier length: " + frontier.length)
    this.logger.trace("isMeetCriterion: " + this.critorion.isMeetCriterion)
    while (result == null && this.frontier.length > 0 && !this.critorion.isMeetCriterion) {
      val candicate = this.frontier.pop
      this.logger.trace("working on path candicate: " + Path(candicate.edges.reverse))
      val currentNode = candicate.edges.head.to
      if (endNodes.contains(currentNode)) {
        result = Path(candicate.edges.reverse)
        this.critorion.addVisitPath(result)
        this.logger.trace("found a complete path:" + result)
      } else {
        val outgoingEdges = graph.outgoingEdges(currentNode)
        if (outgoingEdges.size == 0) {
          result = Path(candicate.edges.reverse)
          this.critorion.addVisitPath(result)
          this.logger.trace("found a path that has not next nodes:" + result)
        } else {
          outgoingEdges.foreach(edge => {
            val newPath = Path(edge :: candicate.edges)
            val newPathTruth = Path(newPath.edges.reverse)
            if (!this.critorion.isVisited(newPathTruth)) {
              this.frontier.add(newPath)
              this.logger.trace("found new path candicate:" + Path(newPath.edges.reverse))
            }
          })
        }
      }
    }
    result
  }
}


sealed trait CoverageCriterion[V, U](graph: Graph[V,U]) {
  def addVisitPath(path: Path[V, U]): Unit
  def isMeetCriterion: Boolean
  def isVisited(path: Path[V, U]): Boolean
}

case class AllNodeCriterion[V, U](val graph: Graph[V,U]) extends CoverageCriterion[V, U](graph) {
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

case class AllEdgeCriterion[V, U](val graph: Graph[V,U]) extends CoverageCriterion[V, U](graph) {
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

case class AllPathCriterion[V, U](val graph: Graph[V,U]) extends CoverageCriterion[V, U](graph) {
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