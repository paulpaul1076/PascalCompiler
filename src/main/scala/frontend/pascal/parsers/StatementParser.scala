package frontend.pascal.parsers

import frontend.pascal.{PascalErrorCode, PascalParserTD, PascalTokenType}
import frontend.{EofToken, Token}
import intermediate.icodeimpl.{ICodeKeyImpl, ICodeNodeTypeImpl}
import intermediate.{ICodeFactory, ICodeNode}

/**
  * Statement parser.
  *
  * @param pascalParser parent parser.
  */
class StatementParser(pascalParser: PascalParserTD) extends PascalParserTD(pascalParser) {

  /**
    * Parses a list of statements.
    *
    * @param toket      the starting token.
    * @param parentNode parent node of this statement's node.
    * @param terminator the token type of ndoe that terminates the list.
    * @param errorCode  errorCode if the terminator token is missing.
    */
  def parseList(toket: Token, parentNode: ICodeNode, terminator: PascalTokenType, errorCode: PascalErrorCode): Unit = {
    // Loop to parse each statement until the END token.
    // or the end of the source file.
    var curToken = toket // because we can't assign anything to "token"
    while (!curToken.isInstanceOf[EofToken] && (curToken.getTokenType != terminator)) {
      //Parse a statement. The parent node adopts the statement node.
      val statementNode = parse(curToken)
      parentNode.addChild(statementNode)
      curToken = currentToken()
      val tokenType = curToken.getTokenType

      // Look for the semicolon between statements.
      if (tokenType == PascalTokenType.SEMICOLON) {
        curToken = nextToken() // consume the ;
      }
      // If at the start of the next assignment statement,
      // then missing a semicolon.
      else if (tokenType == PascalTokenType.IDENTIFIER) {
        PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_SEMICOLON, this)
      }
      // Unexpected token
      else if (tokenType != terminator) {
        PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.UNEXPECTED_TOKEN, this)
        curToken = nextToken()
      }
    }

    // Look for the terminator token.
    if (curToken.getTokenType == terminator) {
      curToken = nextToken() // consume the terminator token
    } else {
      PascalParserTD.errorHandler.flag(curToken, errorCode, this)
    }
  }


  // TODO: Should this be a protected method?

  /**
    * Parse a statement. To be overridden by the specialized statement parser subclasses.
    *
    * @param token the initial token
    * @return the root of the generated parse tree.
    */
  def parse(token: Token): ICodeNode = {
    val statementNode = token.getTokenType match {
      case PascalTokenType.BEGIN =>
        val compoundStatementParser = new CompoundStatementParser(this)
        compoundStatementParser.parse(token)
      case PascalTokenType.IDENTIFIER =>
        val assignmentStatementParser = new AssignmentStatementParser(this)
        assignmentStatementParser.parse(token)
      case _ =>
        ICodeFactory.createICodeNode(ICodeNodeTypeImpl.NO_OP)
    }

    setLineNumber(statementNode, token)
    statementNode
  }

  /**
    * Set the current line number as a statement node attribute.
    *
    * @param node  ICodeNode.
    * @param token the initial token.
    */
  protected def setLineNumber(node: ICodeNode, token: Token): Unit = {
    if (node != null) {
      node.setAttribute(ICodeKeyImpl.LINE, token.getLineNumber)
    }
  }
}
