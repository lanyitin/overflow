package tw.lanyitin.overflow

import org.slf4j.LoggerFactory

import tw.lanyitin.huevo.parse._
import tw.lanyitin.huevo.lex.Scanner
import tw.lanyitin.common.ast.Token
import tw.lanyitin.common.ast.TokenType._
import tw.lanyitin.common.graph.Graph
import tw.lanyitin.common.graph.DirectedEdge
import tw.lanyitin.common.graph.Edge
import tw.lanyitin.common.graph.Node
import tw.lanyitin.common.ast.OperationCallExpression
import tw.lanyitin.common.ast.BooleanLiteralExpression
import tw.lanyitin.common.ast.TokenType.BooleanConstantToken
import tw.lanyitin.common.ast.IdentifierExpression
import tw.lanyitin.common.ast.Expression
import tw.lanyitin.common.ast.FunctionCallExpression
import tw.lanyitin.common.ast.FloatLiteralExpression
import tw.lanyitin.common.ast.IntegerLiteralExpression
import scala.util.Try



trait ModelParser[V, U] {
  val logger = LoggerFactory.getLogger(this.getClass)
  def parse: Set[Graph[V, U]]
  def expr2Node(expr: Expression): Node[V] 
  def pseudoNode: Node[V]
  def pseudoEdge(node1: Node[V], node2: Node[V]): Edge[V, U]
  def isTruePath(edge: DirectedEdge[V, U]): Boolean
  def isFalsePath(edge: DirectedEdge[V, U]): Boolean
  def extractExpression(node: Node[V]): String
  def parseExpression(str: String): Try[Expression] = {
    Parser.parseOnly = true;
    Parser.parse_boolean_expression(Scanner(str)).map(_._1);
  }

  def isBranchNode(node: Node[V], g: Graph[V, U]): Boolean = {
    val outgoingEdges = g.outgoingEdges(node)
    outgoingEdges.exists(this.isTruePath) && outgoingEdges.exists(this.isFalsePath)
  }

  def exprToGraph(expr: Expression): Graph[V, U] = {
    val result = expr match {
      case FunctionCallExpression(_, _) => Graph(Set(this.expr2Node(expr)), Set[Edge[V, U]]())
      case OperationCallExpression(token, expr1, expr2) => {
        if (token.txt == "or") {
          val g1 = exprToGraph(expr1)
          val g2 = exprToGraph(expr2)
          val startNode = this.pseudoNode
          val endNode = this.pseudoNode
          val startToLeft = g1.beginNodes.map(n => this.pseudoEdge(startNode, n)).toSet
          val startToRight = g2.beginNodes.map(n => this.pseudoEdge(startNode, n)).toSet
          val leftToEnd = g1.endNodes.map(n => this.pseudoEdge(n, endNode)).toSet
          val rightToEnd = g2.endNodes.map(n => this.pseudoEdge(n, endNode)).toSet
          val allNodes = g1.nodes.union(g2.nodes) + startNode + endNode
          val allEdges: Set[tw.lanyitin.common.graph.Edge[V,U]] = g1.edges.union(g2.edges).union(startToLeft).union(startToRight).union(leftToEnd).union(rightToEnd)
          Graph(allNodes, allEdges)
        } else if (token.txt == "and") {
          val g1 = exprToGraph(expr1)
          val g2 = exprToGraph(expr2)
          // this.logger.trace(s"=>>>> ${expr2}")

          val allNodes = g1.nodes.union(g2.nodes)
          val allEdges = g1.edges.union(g2.edges)
            .union(for {
              upnode <- g1.endNodes
              downNode <- g2.beginNodes
            } yield this.pseudoEdge(upnode, downNode))
          Graph(allNodes, allEdges)
        } else {
          val result = this.expr2Node(expr)

          // this.logger.trace(s"==============> ${expr} ${result}")
          Graph(Set(result), Set[Edge[V, U]]())
        }
      }
      case default => Graph(Set(this.expr2Node(expr)), Set[Edge[V, U]]())
    }
    this.logger.trace(s"exprToGraph: ${expr} ${result}")
    result
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

  def transformation(graph: Graph[V, U]): Graph[V, U] = {
    def iterate(nodes: List[Node[V]], originGraph: Graph[V, U]): Graph[V, U] = {
      // this.logger.trace("working on graph: " + originGraph)
      if (nodes.length == 0) originGraph
      else {
        val node = nodes.head
        val exprStr = this.extractExpression(node)
        this.logger.trace("working on transform: " + exprStr)
        if (exprStr.length > 0) {
          this.logger.trace(s"expression: ${exprStr}")
          try {
            val parseResult = this.parseExpression(exprStr)
            if (parseResult.isSuccess) {
              val expr = parseResult.get
              val exprGraph = this.exprToGraph(expr)
              // this.logger.trace("expr graph: " + exprGraph)
              val originOutgoingEdge = originGraph.outgoingEdges(node)
              val originIncomeEdge = originGraph.incomingEdges(node)
              if (this.isBranchNode(node, originGraph)) {
                this.logger.trace(s"${node} is a branch")
                val falseExprGraph = this.exprToGraph(this.inverse(expr))
                val beginNode = this.pseudoNode
                
                val newIncomingEdges = 
                originIncomeEdge.map(e => DirectedEdge(e.from, beginNode, e.annotation).asInstanceOf[Edge[V, U]])
                .union(exprGraph.beginNodes.map(n => this.pseudoEdge(beginNode, n)))
                .union(falseExprGraph.beginNodes.map(n => this.pseudoEdge(beginNode, n)))

                val newOutgoingTruePath = originOutgoingEdge.filter(this.isTruePath)
                  .flatMap(e => exprGraph.endNodes.map(endNode => DirectedEdge(endNode, e.to, e.annotation)))

                val newOutgoingFalsePath = originOutgoingEdge.filter(this.isFalsePath)
                  .flatMap(e => falseExprGraph.endNodes.map(endNode => DirectedEdge(endNode, e.to, e.annotation)))

                val newGraph = Graph(
                  originGraph.nodes.union(exprGraph.nodes).union(falseExprGraph.nodes) - node + beginNode,
                  ((((originGraph.edges -- originIncomeEdge) -- originOutgoingEdge) ++ newIncomingEdges) ++ newOutgoingTruePath ++ newOutgoingFalsePath) ++ exprGraph.edges ++ falseExprGraph.edges
                )
                // this.logger.trace("new graph: " + newGraph)
                iterate(nodes.tail, newGraph) 
              } else {
                this.logger.trace(s"${node} is not a branch")
                val newOutgoingEdges = originOutgoingEdge.flatMap(e => exprGraph.endNodes.map(endNode => DirectedEdge(endNode, e.to, e.annotation)))
                val newIncomingEdges = originIncomeEdge.flatMap(e => exprGraph.beginNodes.map(beginNode => DirectedEdge(e.from, beginNode, e.annotation)))
                val newGraph = Graph(
                  originGraph.nodes.union(exprGraph.nodes) - node,
                  ((((originGraph.edges -- originIncomeEdge) -- originOutgoingEdge) ++ newIncomingEdges) ++ newOutgoingEdges) ++ exprGraph.edges
                )
                // this.logger.trace("new graph: " + newGraph)
                iterate(nodes.tail, newGraph)
              }
            } else {
              this.logger.warn("parse node failed: " + parseResult)
              iterate(nodes.tail, originGraph)
            }
          } catch {
            case e: Throwable =>
              iterate(nodes.tail, originGraph)
          }
        } else {
          iterate(nodes.tail, originGraph)
        }
      }
    }
    iterate(graph.nodes.toList, graph)
  }
}