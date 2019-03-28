package frontend.pascal.parsers

import java.util

import frontend.pascal.{PascalErrorCode, PascalParserTD, PascalTokenType}
import frontend.{Parser, Token}
import intermediate.icodeimpl.{ICodeKeyImpl, ICodeNodeTypeImpl}
import intermediate.{ICodeFactory, ICodeNode}

/**
  * Assignment parser.
  *
  * @param pascalParser parent parser.
  */
class AssignmentStatementParser(pascalParser: PascalParserTD) extends StatementParser(pascalParser) {

  /**
    * Parse an assignment statement.
    *
    * @param toket the initial token.
    * @return the root of the generated parse tree.
    */
  override def parse(toket: Token): ICodeNode = {
    // Create an ASSIGN node.
    val assignNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.ASSIGN)
    var curToken = toket
    // Look up the target identifier in the symbol table stack.
    // Enter the identifier into the table if it's not found.
    val targetName = curToken.getText.toLowerCase
    var targetId = Parser.symTabStack.lookup(targetName)
    if (targetId == null) {
      targetId = Parser.symTabStack.enterLocal(targetName)
    }
    targetId.appendLineNumber(curToken.getLineNumber)
    curToken = nextToken() // consume the identifier token

    // Create the variable node and set its name attribute.
    val variableNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.VARIABLE)
    variableNode.setAttribute(ICodeKeyImpl.ID, targetId)

    // The ASSIGN node adopts the variable node as its first child.
    assignNode.addChild(variableNode)

    //Synchronize on the := token.
    curToken = synchronize(AssignmentStatementParser.COLON_EQUALS_SET)

    // Look for the := token.
    if (curToken.getTokenType == PascalTokenType.COLON_EQUALS) {
      curToken = nextToken() // consume the :=
    } else {
      PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_COLON_EQUALS, this)
    }

    // Parse the expression. The ASSIGN node adopts the expression's
    // node as its second child.
    val expressionParser = new ExpressionParser(this)
    assignNode.addChild(expressionParser.parse(curToken))

    assignNode
  }
}

/**
 * Companion object.
 */
private object AssignmentStatementParser {
  val COLON_EQUALS_SET: util.HashSet[PascalTokenType] = ExpressionParser.EXPR_START_SET.clone().asInstanceOf[util.HashSet[PascalTokenType]]
  COLON_EQUALS_SET.add(PascalTokenType.COLON_EQUALS)
  COLON_EQUALS_SET.addAll(StatementParser.STMT_FOLLOW_SET)
}