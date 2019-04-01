package frontend.pascal.parsers

import java.util

import frontend.{Parser, Token}
import frontend.pascal.{PascalErrorCode, PascalParserTD, PascalTokenType}
import intermediate.icodeimpl.ICodeNodeTypeImpl
import intermediate.{ICodeFactory, ICodeNode}

/**
 * While statement parser.
 *
 * @param pascalParserTD parent parser.
 */
class WhileStatementParser(pascalParserTD: PascalParserTD) extends StatementParser(pascalParserTD) {
  /**
   * Parse a while statement.
   *
   * @param toket current token.
   * @return the root of the generated parse tree.
   */
  override def parse(toket: Token): ICodeNode = {
    var curToken = nextToken()

    // Create LOOP, TEST, and NOT nodes.
    val loopNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.LOOP)
    val breakNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.TEST)
    val notNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.NOT)

    // The LOOP node adopts the TEST node as its first child.
    // The TEST node adopts the NOT node as its only child.
    loopNode.addChild(breakNode)
    breakNode.addChild(notNode)

    // Parse the expression.
    // The NOT node adopts the expression subtree as its only child.
    val expressionParser = new ExpressionParser(this)
    notNode.addChild(expressionParser.parse(curToken))

    // Synchronize at the DO.
    curToken = synchronize(WhileStatementParser.DO_SET)
    if (curToken.getTokenType == PascalTokenType.DO) {
      curToken = nextToken() // consume the DO
    } else {
      PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_DO, this)
    }

    // Parse the statements.
    // The LOOP node adopts the statement subtree as its second child.
    val statementParser = new StatementParser(this)
    loopNode.addChild(statementParser.parse(curToken))

    loopNode
  }
}

/**
 * Synchronization set for DO.
 */
object WhileStatementParser {
  val DO_SET = StatementParser.STMT_START_SET.clone().asInstanceOf[util.HashSet[PascalTokenType]]
  DO_SET.add(PascalTokenType.DO)
  DO_SET.addAll(StatementParser.STMT_FOLLOW_SET)
}