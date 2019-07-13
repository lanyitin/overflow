
package tw.lanyitin.overflow

import scala.collection.mutable.Stack
import scala.collection.mutable.Queue
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import tw.lanyitin.common.graph.Graph
import tw.lanyitin.common.graph.Path
import tw.lanyitin.common.graph.DirectedEdge
import tw.lanyitin.common.graph.UndirectedEdge
import tw.lanyitin.common.graph.Node

trait TraversalFrontier[V] {
  def add(node: V): Unit
  def pop: V
  def length: Integer
  def contains(node: V): Boolean
}
case class StackFrontier[V]() extends TraversalFrontier[V] {
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
case class QueueFrontier[V]() extends TraversalFrontier[V] {
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


object TraversalFrontierFactroy {
  def getTraversalFrontier[V](name: String): TraversalFrontier[V] = name match {
    case "bfs" => QueueFrontier()
    case "dfs" => StackFrontier()
  }
}

class PathEnumerator[V, U](val graph: Graph[V, U], val loops: Set[List[Node[V]]], val frontier: TraversalFrontier[Path[V, U]], val critorion: CoverageCriterion[V, U]) {
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
    val beginOfLoops = loops.map(_.head)
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
              if (beginOfLoops.contains(edge.to)) {
                val currentPath = candicate.edges.reverse.head.from :: candicate.edges.reverse.head.to :: candicate.edges.reverse.tail.map(_.to)
                val hasLoop = loops.foldRight(false)((iteration, result) => currentPath.containsSlice(iteration) || result)
                if (!hasLoop) {
                  this.frontier.add(newPath)
                  this.logger.trace("found new path candicate:" + Path(newPath.edges.reverse))
                }
              } else {
                this.frontier.add(newPath)
                this.logger.trace("found new path candicate:" + Path(newPath.edges.reverse))
              }
            }
          })
        }
      }
    }
    result
  }
}