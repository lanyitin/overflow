import tw.lanyitin.overflow._
import org.slf4j.LoggerFactory;
object Main {

  val logger = LoggerFactory.getLogger(this.getClass)

  def main(args: Array[String]): Unit = {
    this.logger.trace("prepare graph")
    val node1 = Node(1)
    val node2 = Node(2)
    val node3 = Node(3)
    val node4 = Node(4)
    val nodes = Set(node1, node2, node3, node4)

    val edge1 = UndirectedEdge(node1, node2)
    val edge2 = UndirectedEdge(node2, node3)
    val edge3 = UndirectedEdge(node3, node4)
    val edge4 = DirectedEdge(node1, node4)

    val edge9 = DirectedEdge(node1, node2)
    val edge5 = DirectedEdge(node2, node3)
    val edge6 = DirectedEdge(node3, node4)

    val edge7 = DirectedEdge(node2, node4)

    val edge8 = DirectedEdge(node3, node2)

    val edges: Set[Edge[Int, Null]] = Set(edge1, edge2, edge3, edge4, edge5, edge6, edge7, edge8, edge9)

    val graph: Graph[Int, Null] = Graph(nodes, edges)


    val frontier: TraversalFrontier[Path[Int, Null]] = new QueueFrontier()

    this.logger.trace("start path enumation");
    val pathEnumerator: PathEnumerator[Int, Null] = new PathEnumerator[Int, Null](graph, frontier)
    println(pathEnumerator.nextPath)
    println(pathEnumerator.nextPath)
    println(pathEnumerator.nextPath)
    println(pathEnumerator.nextPath)
    println(pathEnumerator.nextPath)
    println(pathEnumerator.nextPath)
  }
}
