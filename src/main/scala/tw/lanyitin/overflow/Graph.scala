package tw.lanyitin.overflow
class Node[V] (val payload: V) {

}

class Edge[V] (val node1: Node[V], val node2: Node[V]) {

}

class DirectedEdge[V](val from: Node[V], val to: Node[V]) extends Edge(from, to) {

}

class Graph[V] (val nodes: Set[Node[V]], val edges: Set[Edge[V]]) {
  def adjacentNodes(node: Node[V]): Set[Node[V]] = {
    this.edges
      .filter((edge) => edge.node1 == node || edge.node2 == node)
      .flatMap(edge => List(edge.node1, edge.node2))
      .filter(candicate => candicate != node)
      .toSet
  }

  def childNodes(node: Node[V]): Set[Node[V]] = {
    this.edges
      .flatMap(edge => {
        if (edge.isInstanceOf[DirectedEdge[V]]) {
          List(edge.asInstanceOf[DirectedEdge[V]])
        } else {
          List()
        }
      })
      .filter(diEdge => diEdge.from == node)
      .map(diEdge => diEdge.to)
      .toSet
  }

  def parentNodes(node: Node[V]): Set[Node[V]] = {
    this.edges
      .flatMap(edge => {
        if (edge.isInstanceOf[DirectedEdge[V]]) {
          List(edge.asInstanceOf[DirectedEdge[V]])
        } else {
          List()
        }
      })
      .filter(diEdge => diEdge.to == node)
      .map(diEdge => diEdge.from)
      .toSet
  }
}
