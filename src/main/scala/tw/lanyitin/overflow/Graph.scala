package tw.lanyitin.overflow

import scala.collection.mutable.Stack
import scala.collection.mutable.Queue

class Node[V] (val payload: V) {
  override def toString = this.payload.toString
}

class Edge[V] (val node1: Node[V], val node2: Node[V], val annotation: String = "") { }

class DirectedEdge[V](val from: Node[V], val to: Node[V]) extends Edge(from, to) { }

class Graph[V] (val nodes: Set[Node[V]], val edges: Set[Edge[V]]) {
  def adjacentNodes(node: Node[V]): Set[Node[V]] = {
    this.edges
      .filter((edge) => edge.node1 == node || edge.node2 == node)
      .flatMap(edge => List(edge.node1, edge.node2))
      .filter(candicate => candicate != node)
  }

  def childNodes(node: Node[V]): Set[Node[V]] = {
    this.directedEdges
      .filter(diEdge => diEdge.from == node)
      .map(diEdge => diEdge.to)
  }

  def parentNodes(node: Node[V]): Set[Node[V]] = {
    this.directedEdges
      .filter(diEdge => diEdge.to == node)
      .map(diEdge => diEdge.from)
  }

  def directedEdges: Set[DirectedEdge[V]] = {
    this.edges
      .flatMap(edge => {
        if (edge.isInstanceOf[DirectedEdge[V]]) {
          Set(edge.asInstanceOf[DirectedEdge[V]])
        } else {
          Set()
        }
      })
  }

  def beginNodes: Set[Node[V]] = {
    this.nodes -- (this.directedEdges.map(edge => edge.to))
  }

  def endNodes: Set[Node[V]] = {
    this.nodes -- (this.directedEdges.map(edge => edge.from))
  }

  def isCompletePath(path: List[Node[V]]): Boolean = {
    if (path.length == 0) {
      false
    } else {
      val beginSet = this.beginNodes
      val endSet = this.endNodes
      beginSet.contains(path.head) && endSet.contains(path.last)
    }
  }
}


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
    this.queue :+ node
  }
  def pop: V = {
    this.queue.dequeue
  }
  def contains(node: V): Boolean = {
    this.queue.contains(node)
  }

  def length = this.queue.length
}

class PathEnumerator[V](val graph: Graph[V], val frontier: TraversalFrontier[List[Node[V]]]) {
  val beginNodes = this.graph.beginNodes
  val endNodes = this.graph.endNodes
  if (this.beginNodes.size == 0) {
    throw new Exception("this graph doesn't have beging node");
  }

  this.beginNodes.foreach(node => {
    var candicate: List[Node[V]] = List(node)
    this.frontier.add(candicate)
  })

  var visitPath: List[List[Node[V]]] = List()

  def nextPath: List[Node[V]] = {
    var result: List[Node[V]] = null
    while (result == null && this.frontier.length > 0) {
      val candicate = this.frontier.pop
      val currentNode = candicate.head
      if (endNodes.contains(currentNode)) {
        this.visitPath = candicate :: this.visitPath
        result = candicate.reverse
      } else {
        val nextNodes = graph.childNodes(currentNode)
        if (nextNodes.size == 0) {
          this.visitPath = candicate :: this.visitPath
          result = candicate.reverse
        } else {
          nextNodes.foreach(nextNode => {
            val newPath = nextNode :: candicate
            if (!visitPath.contains(newPath)) {
              this.frontier.add(newPath)
            }
          })
        }
      }
    }
    result
  }
}
