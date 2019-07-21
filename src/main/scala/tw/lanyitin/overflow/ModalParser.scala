package tw.lanyitin.overflow

import org.slf4j.{Logger, LoggerFactory}
import tw.lanyitin.huevo.parse._
import tw.lanyitin.huevo.lex.{Scanner, ScannerBuilder, ScannerState}
import tw.lanyitin.common.ast.{BooleanLiteralExpression, Expression, ExprsBlock, FieldCallExpression, FloatLiteralExpression, FunctionCallExpression, IdentifierExpression, IntegerLiteralExpression, OperationCallExpression, Token}
import tw.lanyitin.common.ast.TokenType._
import tw.lanyitin.common.graph.Graph
import tw.lanyitin.common.graph.DirectedEdge
import tw.lanyitin.common.graph.Path
import tw.lanyitin.common.graph.Edge
import tw.lanyitin.common.graph.Node
import tw.lanyitin.common.graph.GraphFactory
import tw.lanyitin.common.parser.{Failed, ParseResult}
import tw.lanyitin.common.ast.TokenType.BooleanConstantToken

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

case class Config(val model: String, val mcc: Boolean, val criterion: String, val traverse: String, mergeEquivalentEdges: Boolean)

object Helper {
  def inverse(expr: Expression): Expression = expr match  {
    case IdentifierExpression(_, _) | FunctionCallExpression(_, _) | FieldCallExpression(_, _) => OperationCallExpression(Token(EqualToken, EqualToken.toString, 0, 0), expr, BooleanLiteralExpression(Token(BooleanConstantToken, "false", 0, 0), false))
    case OperationCallExpression(token, expr1, expr2) =>
      token.tokenType match {
        case BooleanAndToken => OperationCallExpression(Token(BooleanOrToken, "or", 0, 0), inverse(expr1), inverse(expr2))
        case BooleanOrToken => OperationCallExpression(Token(BooleanAndToken, "and", 0, 0), inverse(expr1), inverse(expr2))
        case GreaterEqualToken  => OperationCallExpression(Token(LessToken, "<", 0, 0), expr1, expr2)
        case LessEqualToken  => OperationCallExpression(Token(GreaterToken, ">", 0, 0), expr1, expr2)
        case NotEqualToken  => OperationCallExpression(Token(EqualToken, "==", 0, 0), expr1, expr2)
        case EqualToken => OperationCallExpression(Token(NotEqualToken, "!=", 0, 0), expr1, expr2)
        case GreaterToken  => OperationCallExpression(Token(LessEqualToken, "<=", 0, 0), expr1, expr2)
        case LessToken  => OperationCallExpression(Token(GreaterEqualToken, ">=", 0, 0), expr1, expr2)
      }
    case BooleanLiteralExpression(token, value) => {
      if (value) {
        BooleanLiteralExpression(Token(BooleanConstantToken, "false", token.line, token.col), false)
      } else {
        BooleanLiteralExpression(Token(BooleanConstantToken, "true", token.line, token.col), true)
      }
    }
  }

  def exprToStr(expr: Expression): String = {
    expr match  {
      case BooleanLiteralExpression(_, value) => value.toString
      case IntegerLiteralExpression(_, value) => value.toString
      case FloatLiteralExpression(_, value) => value.toString
      case IdentifierExpression(token, _) => token.txt
      case FieldCallExpression(source, field) => s"${exprToStr(source)}.${field.token.txt}"
      case FunctionCallExpression(func, parameters @_*) =>
        s"${func.token.txt}(${parameters.map(this.exprToStr).mkString(", ")})"

      case OperationCallExpression(token, expr1, expr2) =>
        s"(${this.exprToStr(expr1)} ${token.txt} ${this.exprToStr(expr2)})"
    }
  }

  def loopDetection[V, U](graph: Graph[V, U]): Set[List[Node[V]]] = {
    // index := 0
    var index = 0
    // S := empty stack
    val stack : mutable.Stack[Node[V]] = mutable.Stack()
    val indexMap: mutable.Map[Node[V], Int] = mutable.Map()
    val lowLinkMap: mutable.Map[Node[V], Int] = mutable.Map()
    val strongConnectedNodes: mutable.Set[Node[V]] = mutable.Set()


    def strongconnect(node: Node[V]): Unit = {
      // // Set the depth index for v to the smallest unused index
      // v.index := index
      indexMap.put(node, index)
      // v.lowlink := index
      lowLinkMap.put(node, index)
      // index := index + 1
      index = index + 1
      // S.push(v)
      stack.push(node)
      // v.onStack := true

      // // Consider successors of v
      // for each (v, w) in E do
      for (edge <- graph.edges.map(_.extractData).filter(_._1 == node)) {
      // if (w.index is undefined) then
        if (indexMap.get(edge._2).isEmpty) {
          // // Successor w has not yet been visited; recurse on it
          // strongconnect(w)
          strongconnect(edge._2)
          // v.lowlink  := min(v.lowlink, w.lowlink)
          lowLinkMap.put(node, Math.min(lowLinkMap(node), lowLinkMap(edge._2)))
        } else if (stack.contains(edge._2)) {
          // else if (w.onStack) then
          // // Successor w is in stack S and hence in the current SCC
          // // If w is not on stack, then (v, w) is a cross-edge in the DFS tree and must be ignored
          // // Note: The next line may look odd - but is correct.
          // // It says w.index not w.lowlink; that is deliberate and from the original paper
          // v.lowlink  := min(v.lowlink, w.index)
          lowLinkMap.put(node, Math.min(lowLinkMap(node), lowLinkMap(edge._2)))
        }
      }
      // // If v is a root node, pop the stack and generate an SCC
      //if (v.lowlink = v.index) then
      if (lowLinkMap(node) == indexMap(node)) {
        //  start a new strongly connected component
        var anotherNode = stack.pop()
        strongConnectedNodes.add(anotherNode)
        while (!stack.isEmpty && anotherNode != node) {
          anotherNode = stack.pop
          strongConnectedNodes.add(anotherNode)
        }
      }
      //end if
    }

    // for each v in V do
    for (node <- graph.nodes) {
      // if (v.index is undefined) then
      if (indexMap.get(node).isEmpty) {
        //  strongconnect(v)
        strongconnect(node)
      }
      // end if
    }
    // end for
    strongConnectedNodes.groupBy(lowLinkMap(_)).values.filter(loop => loop.size > 1)
      .map(loop => loop.map(n => (n, indexMap(n))).toList.sortBy(_._2).map(x => x._1))
      .toSet
  }
}

case class GraphModification[V, U] (run: Graph[V, U] => Graph[V, U])

object GraphModificationOps {
  def unit[V, U]: GraphModification[V, U] = GraphModification(g => g)

  def chain[V, U](m1: GraphModification[V, U], m2: GraphModification[V, U]): GraphModification[V, U] =
    GraphModification(g => {
      val newG = m1.run(g)
      m2.run(newG)
    })
}

case class SemiPath[V](nodes: List[Node[V]], truthy: Boolean)
object SemiPathOps {
  def or[V, U](p1: SemiPath[V], p2: SemiPath[V]): SemiPath[V] = SemiPath(
    p1.nodes ++ p2.nodes,
    p1.truthy || p2.truthy
  )

  def and[V, U](p1: SemiPath[V], p2: SemiPath[V]): SemiPath[V] = SemiPath(
    p1.nodes ++ p2.nodes,
    p1.truthy && p2.truthy
  )
}


trait ModelParser[V, U] { self: GraphFactory[V, U] =>
  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  // abstract methods
  def config: Config
  def parseGraph: Set[(String, Graph[V, U])]
  def expr2Node(expr: Expression): Node[V]
  def isTruePath(edge: DirectedEdge[V, U]): Boolean
  def isFalsePath(edge: DirectedEdge[V, U]): Boolean
  def extractExpression(node: Node[V]): String
  def exprToStr(expr: Expression): String = Helper.exprToStr(expr)

  def mergeEquivalentEdges(edges: Set[Edge[V, U]]): Set[Edge[V, U]]
  def filterOutNoConnectedNodes(nodes: Set[Node[V]], edges: Set[Edge[V, U]]): Set[Node[V]]

  private def scanner(txt: String): Scanner = {
    val tokenizers = Scanner.huevoTokenizers
    ScannerBuilder(tokenizers).normalMode(txt, ScannerState(0, 0, 0))
  }
  def parseExpression(str: String): Try[Expression] = {
    Parsers.parse_program(scanner(str)) match {
      case tw.lanyitin.common.parser.Success(_, result) => result match {
        case ExprsBlock(exprs) => Success(exprs.head)
      }
      case failed@Failed(_, _, _, _) => Failure(new Exception(failed.toString))
    }
  }


  def isBranchNode(node: Node[V], g: Graph[V, U]): Boolean = {
    val outgoingEdges = g.outgoingEdges(node)
    outgoingEdges.exists(this.isTruePath) && outgoingEdges.exists(this.isFalsePath)
  }

  private def productOfTwoSemiPathLists(p1: List[SemiPath[V]], p2: List[SemiPath[V]], op: (SemiPath[V], SemiPath[V]) => SemiPath[V]): List[SemiPath[V]] = {
    for {
      path1 <- p1
      path2 <- p2
    } yield op(path1, path2)
  }

  def isBooleanOrExpression(expr: Expression): Boolean = {
    expr match {
      case OperationCallExpression(token, _, _) => token.tokenType == BooleanOrToken
      case _ => false
    }
  }

  def isBooleanAndExpression(expr: Expression): Boolean = {
    expr match {
      case OperationCallExpression(token, _, _) => token.tokenType == BooleanAndToken
      case _ => false
    }
  }

  def exprToPaths(expr: Expression, truthy: Boolean = true): List[SemiPath[V]] = {
    val result = expr match {
      case OperationCallExpression(token, expr1, expr2) => {
        token.tokenType match {
          case BooleanAndToken | BooleanOrToken => {
            val expr1Inverse = Helper.inverse(expr1)
            val expr2Inverse = Helper.inverse(expr2)

            val expr1Path = this.exprToPaths(expr1).filter(x => x.truthy)
            val expr2Path = this.exprToPaths(expr2).filter(x => x.truthy)

            val expr1InversePath = this.exprToPaths(expr1Inverse, false).flatMap(x => {
              if ((isBooleanOrExpression(expr1Inverse) || isBooleanAndExpression(expr1Inverse)) && x.truthy) {
                Nil
              } else {
                x :: Nil
              }
            })
            val expr2InversePath = this.exprToPaths(expr2Inverse, false).flatMap(x => {
              if ((isBooleanOrExpression(expr2Inverse) || isBooleanAndExpression(expr2Inverse)) && x.truthy) {
                Nil
              } else {
                x :: Nil
              }
            })

            val result = this.productOfTwoSemiPathLists(
              expr1Path ++ expr1InversePath,
              expr2Path ++ expr2InversePath,
              if (token.tokenType == BooleanAndToken) { SemiPathOps.and } else { SemiPathOps.or}
            )
            if (truthy) {
              result
            } else {
              result.map(x => x.copy(truthy = !x.truthy))
            }
          }
          case _ => List(SemiPath(List(this.expr2Node(expr)), truthy))
        }
      }
      case _ => List(SemiPath(List(this.expr2Node(expr)), truthy))
    }
    result
  }

  private def makeRealPath(path: SemiPath[V]): Path[V, U] = {
    // to make sure each node has unique id
    val duplicatePath = SemiPath(path.nodes.map(this.duplicateNode), path.truthy)

    Path(duplicatePath.nodes.zip(duplicatePath.nodes.tail)
      .map(pair => {
        this.logger.trace(s"pair $pair")
        this.pseudoEdge(pair._1, pair._2).asInstanceOf[DirectedEdge[V, U]]
      }))
  }

  def expendExpression(node: Node[V], originGraph: Graph[V, U]): Graph[V, U] = {
    val str = this.extractExpression(node)
    if (str == null || str.length == 0) {
      originGraph
    } else {
      val parseResult = this.parseExpression(str)
      if (!parseResult.isSuccess) {
        this.logger.debug(parseResult.failed.get.toString)
        originGraph
      } else {
        val originOutgoingEdge = originGraph.outgoingEdges(node)
        val originIncomeEdge = originGraph.incomingEdges(node)
        val expr = parseResult.get
        val beginNode = this.pseudoNode
        val incomeToBegin = originIncomeEdge.map(edge => DirectedEdge(edge.from, beginNode, edge.annotation))
        if (this.isBranchNode(node, originGraph)) {
          this.logger.debug(s"$node is a branch")
          val semiPaths = this.exprToPaths(expr)
          this.logger.debug(s"paths from expr: ${expr} ${semiPaths}")
          val truthyPath = (for {
            outTrue <- originOutgoingEdge.filter(this.isTruePath)
            exprTrue <- semiPaths.filter(p => p.truthy)
          } yield {
            val realPath = this.makeRealPath(exprTrue)
            this.logger.trace(s"realPath ${realPath}")

            val beginToPath = this.pseudoEdge(beginNode, realPath.edges.head.from)
            this.logger.trace(s"beginToPath ${beginToPath}")
            val exprToOut = this.pseudoEdge(realPath.edges.last.to, outTrue.to)
            this.logger.trace(s"exprToOut ${exprToOut}")
            List(beginToPath, exprToOut) ++ realPath.edges
          }).flatten
          val falsePath = (for {
            outTrue <- originOutgoingEdge.filter(this.isFalsePath);
            exprTrue <- semiPaths.filter(p => !p.truthy)
          } yield {
            val realPath = this.makeRealPath(exprTrue)
            this.logger.trace(s"realPath ${realPath}")

            val beginToPath = this.pseudoEdge(beginNode, realPath.edges.head.from)
            this.logger.trace(s"beginToPath ${beginToPath}")
            val exprToOut = this.pseudoEdge(realPath.edges.last.to, outTrue.to)
            this.logger.trace(s"exprToOut ${exprToOut}")
            List(beginToPath, exprToOut) ++ realPath.edges
          }).flatten

          val newNodes: Set[Node[V]] = (originGraph.nodes ++
            truthyPath.map(edge => {
              val (from, to, _) = edge.extractData
              Set(from, to)
            }).flatten ++
            falsePath.map(edge => {
              val (from, to, _) = edge.extractData
              Set(from, to)
            }).flatten) - node + beginNode
          val newEdges: Set[Edge[V, U]] = (((originGraph.edges -- originIncomeEdge) -- originOutgoingEdge) ++ truthyPath) ++ falsePath ++ incomeToBegin
          Graph(newNodes, newEdges)
        } else {
          val semiPaths = this.exprToPaths(expr)

          val truthyPath = (for {
            outTrue <- originOutgoingEdge;
            exprTrue <- semiPaths
          } yield {
            val realPath = this.makeRealPath(exprTrue)
            val beginToPath = this.pseudoEdge(beginNode, realPath.edges.head.from)
            val exprToOut = this.pseudoEdge(realPath.edges.last.to, outTrue.to)
            List(beginToPath, exprToOut) ++ realPath.edges
          }).flatten


          val newNodes: Set[Node[V]] = (originGraph.nodes ++
            truthyPath.map(edge => {
              val (from, to, _) = edge.extractData
              Set(from, to)
            }).flatten) - node
          val newEdges: Set[Edge[V, U]] = (((originGraph.edges -- originIncomeEdge) -- originOutgoingEdge) ++ truthyPath) ++ incomeToBegin
          Graph(newNodes, newEdges)
        }
      }
    }
  }

  def transformation(graph: Graph[V, U]): Graph[V, U] =
  {

    val postProcessedEdges = if (this.config.mergeEquivalentEdges) {
      this.mergeEquivalentEdges(graph.edges)
    } else {
      graph.edges
    }

    val postProcessedNodes = this.filterOutNoConnectedNodes(graph.nodes, postProcessedEdges)


    val newGraph = Graph(postProcessedNodes, postProcessedEdges)

    newGraph.nodes.map(node => GraphModification(g => {this.expendExpression(node, g)})).foldLeft(GraphModificationOps.unit[V, U])(GraphModificationOps.chain[V, U]).run(graph)
  }

  def loops(graph: Graph[V, U]): Set[List[Node[V]]] = Helper.loopDetection[V, U](graph)
}
