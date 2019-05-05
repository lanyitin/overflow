import tw.lanyitin.overflow._
import org.slf4j.LoggerFactory
import java.io.File
import org.dom4j.Element


object Main {

  val logger = LoggerFactory.getLogger(this.getClass)

  def main(args: Array[String]): Unit = {
    this.logger.trace("prepare graph")
    var file = new File("/home/lanyitin/Projects/Mega/doc/05-功能需求規格書/20 流程圖/00 登出入首頁/MB-快速登入.xml")
    val parse = DrawIOModalParse(file)
    parse.parse.foreach(graph => {
      this.logger.trace("start path enumation");

      val criterion = AllPathCriterion(graph)
      val frontier: TraversalFrontier[Path[ElementInfo, ElementInfo]] = new QueueFrontier()
      val pathEnumerator: PathEnumerator[ElementInfo, ElementInfo] = new PathEnumerator[ElementInfo, ElementInfo](graph, frontier, criterion)
      while (!criterion.isMeetCriterion && frontier.length != 0) {
        val path = pathEnumerator.nextPath
        if (path != null) {
          this.logger.debug(path.toString)
        }
      }
    })
  }
}