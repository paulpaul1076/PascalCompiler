package frontend.pascal.parsers

import java.util

import frontend.Token
import frontend.pascal.{PascalErrorCode, PascalParserTD, PascalTokenType}
import intermediate.icodeimpl.ICodeNodeTypeImpl
import intermediate.symtabimpl.Predefined
import intermediate.typeimpl.TypeChecker
import intermediate.{ICodeFactory, ICodeNode}

/**
 * IfStatementParser.
 *
 * @param pascalParserTD parent parser.
 */
class IfStatementParser(pascalParserTD: PascalParserTD) extends StatementParser(pascalParserTD) {

  /**
   * Parse an if statement.
   *
   * @param toket start token.
   * @return the root of the generated parse tree.
   */
  override def parse(toket: Token): ICodeNode = {
    var curToken = nextToken() // skip the IF

    // Create an IF node.
    val ifNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.IF)

    // Parse the expression.
    // The IF node adopts the expression subtree as its first child
    val expressionParser = new ExpressionParser(this)
    val exprNode = expressionParser.parse(curToken)
    ifNode.addChild(exprNode)

    // Type check: The expression type must be boolean.
    val exprType = if (exprNode != null) exprNode.getTypeSpec else Predefined.undefinedType

    if (!TypeChecker.isBoolean(exprType)) {
      PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.INCOMPATIBLE_TYPES, this)
    }

    // Synchronize at the THEN.
    curToken = synchronize(IfStatementParser.THEN_SET)
    if (curToken.getTokenType == PascalTokenType.THEN) {
      curToken = nextToken() // consume the THEN
    } else {
      PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_THEN, this)
    }

    // Parse the THEN statement.
    // The IF node adopts the statement subtree as its second child.
    val statementParser = new StatementParser(this)
    ifNode.addChild(statementParser.parse(curToken))
    curToken = currentToken()

    // Look for an ELSE.
    // TODO: should we add synchronize here? or is it not added because ELSE is optional?
    if (curToken.getTokenType == PascalTokenType.ELSE) {
      curToken = nextToken() // consume the THEN

      // Parse the ELSE statement.
      // The IF node adopts the statement subtree as its third child.
      ifNode.addChild(statementParser.parse(curToken))
    }

    ifNode
  }
}

// Companion object.
object IfStatementParser {
  val THEN_SET = StatementParser.STMT_START_SET.clone().asInstanceOf[util.HashSet[PascalTokenType]]
  THEN_SET.add(PascalTokenType.THEN)
  THEN_SET.addAll(StatementParser.STMT_FOLLOW_SET)
}
