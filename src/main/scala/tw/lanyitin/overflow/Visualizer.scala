package tw.lanyitin.overflow

import tw.lanyitin.overflow.Graph

trait Visualizer[V, U, G, P, E, N] {
  def visualize(graph: Graph[V, U]): G
  def visualize(path: Path[V, U]): P
  def visualize(edge: Edge[V, U]): E
  def visualize(node: Node[V]): N
}