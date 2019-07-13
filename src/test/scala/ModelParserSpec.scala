package tw.lanyitin.overflow

import java.io.File

import org.scalatest._
import tw.lanyitin.common.ast.TokenType.TokenType
import tw.lanyitin.common.ast.{BooleanLiteralExpression, Expression, IdentifierExpression, OperationCallExpression, Token, TokenType}

class ModelParserSpec extends FunSuite with Matchers {
  val modelParser = DrawIOModalParser(new File(this.getClass.getClassLoader.getResource("otp.xml").getFile), true)
  val falseConstantExpression = new BooleanLiteralExpression(Token(TokenType.BooleanConstantToken, "false", 0, 0), false)
  val trueConstantExpression = new BooleanLiteralExpression(Token(TokenType.BooleanConstantToken, "true", 0, 0), true)
  val identifierExpression = new IdentifierExpression(Token(TokenType.IdentifierToken, "variable", 0, 0), null)
  val identifierExpression2 = new IdentifierExpression(Token(TokenType.IdentifierToken, "variable2", 0, 0), null)
  val relationOperators: List[TokenType] = List(
    TokenType.GreaterToken,
    TokenType.GreaterEqualToken,
    TokenType.LessToken,
    TokenType.LessEqualToken,
    TokenType.EqualToken,
    TokenType.NotEqualToken,
  )

  def generateRelationOperationExpression(tokenType: TokenType, expr1: Expression, expr2: Expression): OperationCallExpression = {
    OperationCallExpression(Token(tokenType, tokenType.toString, 0, 0), expr1, expr2)
  }

  def generateOrOperationExpression(expr1: Expression, expr2: Expression): OperationCallExpression = {
    OperationCallExpression(Token(TokenType.BooleanOrToken, "or", 0, 0), expr1, expr2)
  }

  def generateAndOperationExpression(expr1: Expression, expr2: Expression): OperationCallExpression = {
    OperationCallExpression(Token(TokenType.BooleanAndToken, "and", 0, 0), expr1, expr2)
  }

  test("exprToPath should able to handle boolean literal(true)") {
    val result = modelParser.exprToPaths(trueConstantExpression)
    result match {
      case Nil => fail("should have at lease one path in list")
      case SemiPath(nodes, _) :: _ => nodes match {
        case Nil => fail("should have at least one node in path")
        case node :: _ => {
          if (node.payload.content != "true") {
            fail("the content of node should be true")
          }
        }
      }
    }
  }

  test("exprToPath should able to handle boolean literal(false)") {
    val result = modelParser.exprToPaths(falseConstantExpression)
    result match {
      case Nil => fail("should have at lease one path in list")
      case SemiPath(nodes, _) :: _ => nodes match {
        case Nil => fail("should have at least one node in path")
        case node :: _ => {
          if (node.payload.content != "false") {
            fail("the content of node should be false")
          }
        }
      }
    }
  }

  test("exprToPath should able to handle identifier expression") {
    val result = modelParser.exprToPaths(identifierExpression)
    result match {
      case Nil => fail("should have at lease one path in list")
      case SemiPath(nodes, _) :: _ => nodes match {
        case Nil => fail("should have at least one node in path")
        case node :: _ => {
          if (node.payload.content != identifierExpression.token.txt) {
            fail("the content of node should be false")
          }
        }
      }
    }
  }

  test("exprToPath should able to handle boolean and operation with two boolean constant as operands") {
    val result = modelParser.exprToPaths(this.generateAndOperationExpression(trueConstantExpression, falseConstantExpression))
    if (result.length != 4) {
      fail("there should be four paths")
    } else {
      val firstPath =  result(0)
      val secondPath = result(1)
      val thirdPath =  result(2)
      val forthPath =  result(3)

      firstPath match {
        case SemiPath(nodes, truthy) => {
          if (!truthy) {
            fail("the truthy of first path should be true")
          } else {
            if (nodes(0).payload.content != "true" || nodes(1).payload.content != "false") {
              fail(s"the content of nodes were wrong ${nodes.map(_.payload.content)}")
            }
          }
        }
      }

      secondPath match {
        case SemiPath(nodes, truthy) => {
          if (truthy) {
            fail("the truthy of second path should be false")
          } else {
            if (nodes(0).payload.content != "true" || nodes(1).payload.content != "true") {
              fail(s"the content of nodes were wrong ${nodes.map(_.payload.content)}")
            }
          }
        }
      }

      thirdPath match {
        case SemiPath(nodes, truthy) => {
          if (truthy) {
            fail("the truthy of third path should be false")
          } else {
            if (nodes(0).payload.content != "false" || nodes(1).payload.content != "false") {
              fail(s"the content of nodes were wrong ${nodes.map(_.payload.content)}")
            }
          }
        }
      }

      forthPath match {
        case SemiPath(nodes, truthy) => {
          if (truthy) {
            fail("the truthy of forth path should be false")
          } else {
            if (nodes(0).payload.content != "false" || nodes(1).payload.content != "true") {
              fail(s"the content of nodes were wrong ${nodes.map(_.payload.content)}")
            }
          }
        }
      }
    }
  }



  test("exprToPath should able to handle boolean or operation with two boolean constant as operands") {
    val result = modelParser.exprToPaths(this.generateOrOperationExpression(trueConstantExpression, falseConstantExpression))
    if (result.length != 4) {
      fail("there should be four paths")
    } else {
      val firstPath =  result(0)
      val secondPath = result(1)
      val thirdPath =  result(2)
      val forthPath =  result(3)

      firstPath match {
        case SemiPath(nodes, truthy) => {
          if (!truthy) {
            fail("the truthy of first path should be true")
          } else {
            if (nodes(0).payload.content != "true" || nodes(1).payload.content != "false") {
              fail(s"the content of nodes were wrong ${nodes.map(_.payload.content)}")
            }
          }
        }
      }

      secondPath match {
        case SemiPath(nodes, truthy) => {
          if (!truthy) {
            fail("the truthy of second path should be true")
          } else {
            if (nodes(0).payload.content != "true" || nodes(1).payload.content != "true") {
              fail(s"the content of nodes were wrong ${nodes.map(_.payload.content)}")
            }
          }
        }
      }

      thirdPath match {
        case SemiPath(nodes, truthy) => {
          if (!truthy) {
            fail("the truthy of third path should be true")
          } else {
            if (nodes(0).payload.content != "false" || nodes(1).payload.content != "false") {
              fail(s"the content of nodes were wrong ${nodes.map(_.payload.content)}")
            }
          }
        }
      }

      forthPath match {
        case SemiPath(nodes, truthy) => {
          if (truthy) {
            fail("the truthy of forth path should be false")
          } else {
            if (nodes(0).payload.content != "false" || nodes(1).payload.content != "true") {
              fail(s"the content of nodes were wrong ${nodes.map(_.payload.content)}")
            }
          }
        }
      }
    }
  }

  test("exprToPaths should able to handle relation operation expression") {
    for (tpy <- this.relationOperators) {
      val expression = this.generateRelationOperationExpression(tpy, identifierExpression, identifierExpression2)
      val result = modelParser.exprToPaths(expression)
      result match {
        case Nil => fail("should have at lease one path in list")
        case SemiPath(nodes, truthy) :: Nil => {
          if (!truthy) {
            fail("the default truthy is true")
          } else {
            if (nodes.length != 1) {
              fail("there should be only one node in path")
            } else {
              val node = nodes.head
              if (node.payload.content != s"(variable ${tpy.toString} variable2)") {
                fail(node.payload.content)
              }
            }
          }
        }
      }
    }
  }
}
