package tw.lanyitin.overflow

import tw.lanyitin.overflow.Graph

trait Visualizer[V, U] {
  def visualize(graph: Graph[V, U]): String
  def visualize(path: Path[V, U]): String
}