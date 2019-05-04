
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

class PathEnumerator[V, U](val graph: Graph[V, U], val frontier: TraversalFrontier[Path[V, U]]) {
  val logger: Logger = LoggerFactory.getLogger(this.getClass)
  val beginNodes = this.graph.beginNodes
  val endNodes = this.graph.endNodes
  var visitedPath: List[Path[V, U]] = List()

  this.logger.trace("number of begin nodes: " + beginNodes.size)

  this.beginNodes.foreach(node1 => {
    val outgoingEdges = graph.outgoingEdges(node1)
    outgoingEdges.foreach(edge => {
      var candicate: Path[V, U] = Path(List(edge))
      this.frontier.add(candicate)
    })
  })

  def nextPath: Path[V,U] = {
    var result: Path[V,U] = null
    while (result == null && this.frontier.length > 0) {
      val candicate = this.frontier.pop
      this.logger.debug("working on path candicate: " + candicate)
      val currentNode = candicate.edges.head.to
      if (endNodes.contains(currentNode)) {
        this.visitedPath = candicate :: this.visitedPath
        result = Path(candicate.edges.reverse)
        this.logger.debug("found a complete path:" + result)
      } else {
        val outgoingEdges = graph.outgoingEdges(currentNode)
        if (outgoingEdges.size == 0) {
          this.visitedPath = candicate :: this.visitedPath
          result = Path(candicate.edges.reverse)
          this.logger.debug("found a path that has not next nodes:" + result)
        } else {
          outgoingEdges.foreach(edge => {
            val newPath = Path(edge :: candicate.edges)
            if (!visitedPath.contains(newPath)) {
              this.frontier.add(newPath)
              this.logger.debug("found new path candicate:" + Path(newPath.edges.reverse))
            }
          })
        }
      }
    }
    result
  }
}
