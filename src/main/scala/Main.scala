import tw.lanyitin.overflow._
object Main {

  def main(args: Array[String]): Unit = {
    val node1 = Node(1)
    val node2 = Node(2)
    val node3 = Node(3)
    val node4 = Node(4)
    val nodes = Set(node1, node2, node3, node4)

    val edge1 = Edge(node1, node2)
    val edge2 = Edge(node2, node3)
    val edge3 = Edge(node3, node4)
    val edge4 = DirectedEdge(node1, node4)

    val edge9 = DirectedEdge(node1, node2)
    val edge5 = DirectedEdge(node2, node3)
    val edge6 = DirectedEdge(node3, node4)

    val edge7 = DirectedEdge(node2, node4)

    val edge8 = DirectedEdge(node3, node2)

    val edges = Set(edge1, edge2, edge3, edge4, edge5, edge6, edge7, edge8, edge9)

    val graph = Graph(nodes, edges)
    val frontier: StackFrontier[List[Node[Int]]] = new StackFrontier()
    val pathEnumerator: PathEnumerator[Int] = new PathEnumerator[Int](graph, frontier)
    println(pathEnumerator.nextPath)
    println(pathEnumerator.nextPath)
    println(pathEnumerator.nextPath)
    println(pathEnumerator.nextPath)
    println(pathEnumerator.nextPath)
    println(pathEnumerator.nextPath)
  }

  def msg = "I was compiled by dotty :)"

}
