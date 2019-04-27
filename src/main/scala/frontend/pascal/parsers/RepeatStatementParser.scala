package frontend.pascal.parsers

import frontend.Token
import frontend.pascal.{PascalErrorCode, PascalParserTD, PascalTokenType}
import intermediate.icodeimpl.ICodeNodeTypeImpl
import intermediate.symtabimpl.Predefined
import intermediate.typeimpl.TypeChecker
import intermediate.{ICodeFactory, ICodeNode}

/**
 * RepeatStatement parser
 *
 * @param pascalParserTD parent parser.
 */
class RepeatStatementParser(pascalParserTD: PascalParserTD) extends StatementParser(pascalParserTD) {
  /**
   * Parse a repeat statement
   *
   * @param toket the beginning token.
   * @return the root of the generated parse tree.
   */
  override def parse(toket: Token): ICodeNode = {
    var curToken = nextToken() // Consume the REPEAT

    // Create the LOOP and TEST nodes.
    val loopNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.LOOP)
    val testNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.TEST)

    // Parse the statement list terminated by the UNTIL token.
    // The LOOP node is the parent of the statement subtrees.
    val statementParser = new StatementParser(this)
    statementParser.parseList(toket = curToken, parentNode = loopNode, terminator = PascalTokenType.UNTIL, errorCode = PascalErrorCode.MISSING_UNTIL)
    curToken = currentToken()

    // Parse the expression.
    // The TEST node adopts the expression subtree as its only child.
    // The LOOP node adopts the TEST node.
    val expressionParser = new ExpressionParser(this)
    val exprNode = expressionParser.parse(curToken)
    testNode.addChild(exprNode)

    // Type check: The test expression must be boolean.
    val exprType = if (exprNode != null) exprNode.getTypeSpec else Predefined.undefinedType

    if (!TypeChecker.isBoolean(exprType)) {
      PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.INCOMPATIBLE_TYPES, this)
    }
    loopNode.addChild(testNode)

    loopNode
  }
}
