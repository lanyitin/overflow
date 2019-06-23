package tw.lanyitin.overflow

import org.slf4j.LoggerFactory

import tw.lanyitin.huevo.parse._
import tw.lanyitin.huevo.lex.Scanner
import tw.lanyitin.common.ast.Token
import tw.lanyitin.common.ast.TokenType._
import tw.lanyitin.common.graph.Graph
import tw.lanyitin.common.graph.DirectedEdge
import tw.lanyitin.common.graph.Path
import tw.lanyitin.common.graph.Edge
import tw.lanyitin.common.graph.Node
import tw.lanyitin.common.graph.GraphFactory
import tw.lanyitin.common.parser.ParseResult
import tw.lanyitin.common.ast.OperationCallExpression
import tw.lanyitin.common.ast.BooleanLiteralExpression
import tw.lanyitin.common.ast.TokenType.BooleanConstantToken
import tw.lanyitin.common.ast.IdentifierExpression
import tw.lanyitin.common.ast.Expression
import tw.lanyitin.common.ast.FunctionCallExpression
import tw.lanyitin.common.ast.FloatLiteralExpression
import tw.lanyitin.common.ast.IntegerLiteralExpression
import scala.util.{Try, Success, Failure}


object Helper {
  def inverse(expr: Expression): Expression = {
    expr match  {
      case IdentifierExpression(_, _) | FunctionCallExpression(_, _) => OperationCallExpression(Token(EqualToken, EqualToken.toString, 0, 0), expr, BooleanLiteralExpression(Token(BooleanConstantToken, "false", 0, 0), false))
      case OperationCallExpression(token, expr1, expr2) => {
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
      }
    }
  }

  def exprToStr(expr: Expression): String = {
    expr match  {
      case BooleanLiteralExpression(_, value) => value.toString
      case IntegerLiteralExpression(_, value) => value.toString
      case FloatLiteralExpression(_, value) => value.toString
      case IdentifierExpression(token, _) => token.txt
      case FunctionCallExpression(func, parameters @_*) => {
        s"${func.token.txt}(${parameters.map(this.exprToStr).mkString(", ")})"
      }
      case OperationCallExpression(token, expr1, expr2) => {
        s"(${this.exprToStr(expr1)} ${token.txt} ${this.exprToStr(expr2)})"
      }
    }
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
  val logger = LoggerFactory.getLogger(this.getClass)
  def exprToStr(expr: Expression) = Helper.exprToStr(expr)

  def parseGraph: Set[Graph[V, U]]
  def expr2Node(expr: Expression): Node[V]

  def isTruePath(edge: DirectedEdge[V, U]): Boolean
  def isFalsePath(edge: DirectedEdge[V, U]): Boolean
  def extractExpression(node: Node[V]): String
  def parseExpression(str: String): Try[Expression] = {
    Parsers.parse_boolean_expression(Scanner(str)) match {
      case Right(ParseResult(state, result)) => Success(result)
      case Left(errors) => Failure(new Exception(errors.mkString("\n")))
    };
  }

  def isBranchNode(node: Node[V], g: Graph[V, U]): Boolean = {
    val outgoingEdges = g.outgoingEdges(node)
    outgoingEdges.exists(this.isTruePath) && outgoingEdges.exists(this.isFalsePath)
  }

  private def productOfTwoSemiPathLists(p1: List[SemiPath[V]], p2: List[SemiPath[V]], op: (SemiPath[V], SemiPath[V]) => SemiPath[V]): List[SemiPath[V]] = {
    for {
      path1 <- p1;
      path2 <- p2
    } yield op(path1, path2)
  }

  def exprToPaths(expr: Expression, truthy: Boolean = true): List[SemiPath[V]] = {
    val result = expr match {
      case OperationCallExpression(token, expr1, expr2) => {
        token.tokenType match {
          case BooleanAndToken => {
            productOfTwoSemiPathLists(
              this.exprToPaths(expr1),
              this.exprToPaths(expr2),
              SemiPathOps.and
            )
          }
          case BooleanOrToken => {
            val expr1Inverse = Helper.inverse(expr1)
            val expr2Inverse = Helper.inverse(expr2)

            val expr1Path = this.exprToPaths(expr1).filter(x => x.truthy == true)
            val expr2Path = this.exprToPaths(expr2).filter(x => x.truthy == true)
            val expr1InversePath = this.exprToPaths(expr1Inverse).map(x => x.copy(truthy = false))
            val expr2InversePath = this.exprToPaths(expr2Inverse).map(x => x.copy(truthy = false))

            this.logger.trace(s"expr1 paths: ${expr1Path ++ expr1InversePath}")
            this.logger.trace(s"expr2 paths: ${expr2Path ++ expr2InversePath}")
            this.logger.trace("\n")

            this.productOfTwoSemiPathLists(
              expr1Path ++ expr1InversePath,
              expr2Path ++ expr2InversePath,
              SemiPathOps.or
            )         
          }
          case default => List(SemiPath(List(this.expr2Node(expr)), truthy))
        }
      }
      case default => List(SemiPath(List(this.expr2Node(expr)), truthy))
    }
    result
  }

  private def makeRealPath(path: SemiPath[V]): Path[V, U] = {
    // to make sure each node has uniqure id
    val duplicatePath = SemiPath(path.nodes.map(this.duplicateNode(_)), path.truthy)

    Path(duplicatePath.nodes.zip(duplicatePath.nodes.tail)
      .map(pair => {
        this.logger.trace(s"pair ${pair}")
        this.pseudoEdge(pair._1, pair._2).asInstanceOf[DirectedEdge[V, U]]
      }))
  }

  def expendExpression(node: Node[V], originGraph: Graph[V, U]): Graph[V, U] = {
    val str = this.extractExpression(node)
    val parseResult = this.parseExpression(str)
    if (!parseResult.isSuccess) {
      originGraph
    } else {
      val originOutgoingEdge = originGraph.outgoingEdges(node)
      val originIncomeEdge = originGraph.incomingEdges(node)
      val expr = parseResult.get
      val beginNode = this.pseudoNode
      val incomeToBegin = originIncomeEdge.map(edge => this.pseudoEdge(edge.from, beginNode))
      if (this.isBranchNode(node, originGraph)) {
        this.logger.trace(s"${node} is a branch")
        val semiPaths = this.exprToPaths(expr)

        val truthyPath = (for {
          outTrue <- originOutgoingEdge.filter(this.isTruePath);
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

  def transformation(graph: Graph[V, U]): Graph[V, U] = {
    graph.nodes.map(node => GraphModification(g => {this.expendExpression(node, g)})).foldLeft(GraphModificationOps.unit[V, U])(GraphModificationOps.chain[V, U]).run(graph)
  }
}
