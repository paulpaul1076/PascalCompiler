package frontend.pascal.parsers

import frontend.Token
import frontend.pascal.{PascalErrorCode, PascalParserTD, PascalTokenType}
import intermediate.icodeimpl.ICodeNodeTypeImpl
import intermediate.{ICodeFactory, ICodeNode}

class RepeatStatementParser(pascalParserTD: PascalParserTD) extends StatementParser(pascalParserTD) {
  override def parse(toket: Token): ICodeNode = {
    var curToken = nextToken() // Consume the REPEAT

    // Create the LOOP and TEST nodes.
    val loopNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.LOOP)
    val testNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.TEST)

    // Parse the statement list terminated by the UNTIL token.
    // The LOOP node is the parent of the statement subtrees.
    val statementParser = new StatementParser(this)
    statementParser.parseList(curToken, loopNode, PascalTokenType.UNTIL, PascalErrorCode.MISSING_UNTIL)
    curToken = currentToken()

    // Parse the expression.
    // The TEST node adopts the expression subtree as its only child.
    // The LOOP node adopts the TEST node.
    val expressionParser = new ExpressionParser(this)
    testNode.addChild(expressionParser.parse(curToken))
    loopNode.addChild(testNode)

    loopNode
  }
}
