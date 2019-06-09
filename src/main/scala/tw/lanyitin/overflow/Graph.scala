package tw.lanyitin.overflow

case class Node[V] (val payload: V)

sealed trait Edge[V, U]
case class UndirectedEdge[V, U](val node1: Node[V], val node2: Node[V], val annotation: U = null) extends Edge[V, U]
case class DirectedEdge[V, U](val from: Node[V], val to: Node[V], val annotation: U = null) extends Edge[V, U]

case class Path[V, U](edges: List[DirectedEdge[V, U]]) {
  override def toString: String = {
    val head = this.edges.head
    (head.from + " -" + head.annotation + "-> " + head.to :: this.edges.tail.map(edge => " -" + edge.annotation + "-> " + edge.to))
    .mkString
  }
}

case class Graph[V, U] (val nodes: Set[Node[V]], val edges: Set[Edge[V, U]]) {
  def adjacentNodes(node: Node[V]): Set[Node[V]] = {
    this.edges
      .flatMap(edge => edge match {
        case UndirectedEdge(node1, node2, payload) => {
          if (node1 == node) {
            List(node2)
          } else if (node2 == node) {
            List(node1)
          } else {
            List()
          }
        }
        case DirectedEdge(from, to, payload) => if (from == node) List(to) else List()
      })
      .toSet
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

  def outgoingEdges(node: Node[V]): Set[DirectedEdge[V, U]] = {
    val edgesFromDirected = this.directedEdges
      .filter(diEdge => diEdge.from == node)
      .toSet

    val edgesFromUndirected = this.undirectedEdges
      .flatMap(edge => {
        if (edge.node1 == node) {
          List(DirectedEdge(edge.node1, edge.node2, edge.annotation))
        } else if (edge.node2 == node) {
          List(DirectedEdge(edge.node2, edge.node1, edge.annotation))
        } else {
          List()
        }
      }).toSet
    edgesFromDirected.union(edgesFromUndirected)
  }

  def incomingEdges(node: Node[V]): Set[DirectedEdge[V, U]] = {
    val edgesFromDirected = this.directedEdges
      .filter(diEdge => diEdge.to == node)
      .toSet

    val edgesFromUndirected = this.undirectedEdges
      .flatMap(edge => {
        if (edge.node1 == node) {
          List(DirectedEdge(edge.node1, edge.node2, edge.annotation))
        } else if (edge.node2 == node) {
          List(DirectedEdge(edge.node2, edge.node1, edge.annotation))
        } else {
          List()
        }
      }).toSet
    edgesFromDirected.union(edgesFromUndirected)
  }

  def undirectedEdges: Set[UndirectedEdge[V, U]] = {
    this.edges
      .flatMap(edge => {
        if (edge.isInstanceOf[UndirectedEdge[V, U]]) {
          Set(edge.asInstanceOf[UndirectedEdge[V, U]])
        } else {
          Set()
        }
      })
  }

  def directedEdges: Set[DirectedEdge[V, U]] = {
    this.edges
      .flatMap(edge => {
        if (edge.isInstanceOf[DirectedEdge[V, U]]) {
          Set(edge.asInstanceOf[DirectedEdge[V, U]])
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

  def isCompletePath(path: Path[V, U]): Boolean = {
    if (path.edges.length == 0) {
      false
    } else {
      val beginSet = this.beginNodes
      val endSet = this.endNodes
      beginSet.contains(path.edges.head.from) && endSet.contains(path.edges.last.to)
    }
  }
}