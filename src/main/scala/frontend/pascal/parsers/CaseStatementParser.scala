package frontend.pascal.parsers

import java.util

import frontend.pascal.{PascalErrorCode, PascalParserTD, PascalTokenType}
import frontend.{EofToken, Token, TokenType}
import intermediate.icodeimpl.{ICodeKeyImpl, ICodeNodeTypeImpl}
import intermediate.{ICodeFactory, ICodeNode}

/**
 * CaseStatementParser.
 *
 * @param pascalParserTD parent parser.
 */
class CaseStatementParser(pascalParserTD: PascalParserTD) extends StatementParser(pascalParserTD) {

  /**
   * Parse a case statement.
   *
   * @param toket starting token.
   * @return the root of the generated parse tree.
   */
  override def parse(toket: Token): ICodeNode = {
    var curToken = nextToken() // consume the CASE

    // Create a SELECT node.
    val selectNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.SELECT)

    // Parse the CASE expression.
    // The SELECT node adopts the expression subtree as its first child.
    val expressionParser = new ExpressionParser(this)
    selectNode.addChild(expressionParser.parse(curToken))

    // Synchronize at the OF.
    curToken = synchronize(CaseStatementParser.OF_SET)
    if (curToken.getTokenType == PascalTokenType.OF) {
      curToken = nextToken()
    } else {
      PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_OF, this)
    }

    // Set of CASE branch constants
    val constantSet = new util.HashSet[Any]()

    // Loop to parse each CASE branch until the END token
    // or the end of the source file.
    while (!curToken.isInstanceOf[EofToken] && (curToken.getTokenType != PascalTokenType.END)) {
      // The SELECT node adopts the CASE branch subtree.
      selectNode.addChild(parseBranch(curToken, constantSet))

      curToken = currentToken()
      val tokenType = curToken.getTokenType

      // Look for the semicolon between CASE branches.
      if (tokenType == PascalTokenType.SEMICOLON) {
        curToken = nextToken() // consume the ;
      }

      // if at the start of the next constant, then missing a semicolon.
      else if (CaseStatementParser.CONSTANT_START_SET.contains(tokenType)) {
        PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_SEMICOLON, this)
      }
    }

    // Look for the END token.
    if (curToken.getTokenType == PascalTokenType.END) {
      curToken = nextToken() // consume END
    } else {
      PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_END, this)
    }

    selectNode
  }

  private def parseBranch(toket: Token, constantSet: util.HashSet[Any]): ICodeNode = {
    var curToken = toket
    // Create a SELECT_BRANCH node and a SELECT_CONSTANTS node.
    // The SELECT_BRANCH node adopts the SELECT_CONSTANTS node as its first child.
    val branchNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.SELECT_BRANCH)
    val constantsNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.SELECT_CONSTANTS)

    branchNode.addChild(constantsNode)

    // Parse the list of CASE branch constants.
    // The SELECT_CONSTANTS node adopts each constant.
    parseConstantList(curToken, constantsNode, constantSet)

    // Look for the : token.
    curToken = currentToken()
    if (curToken.getTokenType == PascalTokenType.COLON) {
      curToken = nextToken() // consume the :
    } else {
      PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_COLON, this)
    }

    // Parse the CASE branch statement. The SELECT_BRANCH node adopts
    // the statement subtree as its second child.
    val statementParser = new StatementParser(this)
    branchNode.addChild(statementParser.parse(curToken))

    branchNode
  }

  private def parseConstantList(toket: Token, constantsNode: ICodeNode, constantSet: util.HashSet[Any]): Unit = {
    var curToken = toket
    // Loop to parse each constant.
    while (CaseStatementParser.CONSTANT_START_SET.contains(curToken.getTokenType)) {
      // The constants list node adopts the constant node.
      constantsNode.addChild(parseConstant(curToken, constantSet))

      //Synchronize at the comma between constants.
      curToken = synchronize(CaseStatementParser.COMMA_SET)

      // Look for the comma.
      if (curToken.getTokenType == PascalTokenType.COMMA) {
        curToken = nextToken() // consume the ,
      }

      // If at the start of the next constant, then missing a comma
      else if (CaseStatementParser.CONSTANT_START_SET.contains(curToken.getTokenType)) {
        PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_COMMA, this)
      }
    }
  }

  private def parseConstant(token: Token, constantSet: util.HashSet[Any]): ICodeNode = {
    var sign: TokenType = null
    var constantNode: ICodeNode = null

    // Synchronize at the start of a constant.
    var curToken = synchronize(CaseStatementParser.CONSTANT_START_SET)
    val tokenType = curToken.getTokenType

    // Plus or minus sign?
    if ((tokenType == PascalTokenType.PLUS) || (tokenType == PascalTokenType.MINUS)) {
      sign = tokenType
      curToken = nextToken() // consume sign
    }

    // Parse the constant.
    curToken.getTokenType.asInstanceOf[PascalTokenType] match {
      case PascalTokenType.IDENTIFIER => constantNode = parseIdentifierConstant(curToken, sign)
      case PascalTokenType.INTEGER    => constantNode = parseIntegerConstant(curToken.getText, sign)
      case PascalTokenType.STRING     => constantNode = parseCharacterConstant(curToken, curToken.getValue.asInstanceOf[String], sign)
      case _                          => PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.INVALID_CONSTANT, this)
    }

    // Check for reused constants.
    if (constantNode != null) {
      val value = constantNode.getAttribute(ICodeKeyImpl.VALUE)

      if (constantSet.contains(value)) {
        PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.CASE_CONSTANT_REUSED, this)
      } else {
        constantSet.add(value)
      }
    }

    nextToken() // consume the constant
    constantNode
  }

  private def parseIdentifierConstant(token: Token, tokenType: TokenType): ICodeNode = {
    // Placeholder: don't allow for now.
    PascalParserTD.errorHandler.flag(token, PascalErrorCode.INVALID_CONSTANT, this)
    null
  }

  private def parseIntegerConstant(value: String, sign: TokenType): ICodeNode = {
    val constantNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.INTEGER_CONSTANT)
    var intValue = Integer.parseInt(value)

    if (sign == PascalTokenType.MINUS) {
      intValue = -intValue
    }

    constantNode.setAttribute(ICodeKeyImpl.VALUE, intValue)
    constantNode
  }

  private def parseCharacterConstant(token: Token, value: String, sign: TokenType): ICodeNode = {
    var constantNode: ICodeNode = null

    if (sign != null) {
      PascalParserTD.errorHandler.flag(token, PascalErrorCode.INVALID_CONSTANT, this)
    } else {
      if (value.length == 1) {
        constantNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.STRING_CONSTANT)
        constantNode.setAttribute(ICodeKeyImpl.VALUE, value)
      } else {
        PascalParserTD.errorHandler.flag(token, PascalErrorCode.INVALID_CONSTANT, this)
      }
    }
    constantNode
  }

}


/**
 * Companion object for this class.
 */
object CaseStatementParser {
  // Synchronization set for starting a CASE option constant.
  val CONSTANT_START_SET = new util.HashSet[PascalTokenType]()
  CONSTANT_START_SET.add(PascalTokenType.IDENTIFIER)
  CONSTANT_START_SET.add(PascalTokenType.INTEGER)
  CONSTANT_START_SET.add(PascalTokenType.PLUS)
  CONSTANT_START_SET.add(PascalTokenType.MINUS)
  CONSTANT_START_SET.add(PascalTokenType.STRING)

  // Synchronization set for OF.
  val OF_SET = CONSTANT_START_SET.clone().asInstanceOf[util.HashSet[PascalTokenType]]
  OF_SET.add(PascalTokenType.OF)
  OF_SET.addAll(StatementParser.STMT_FOLLOW_SET)

  // Synchronization
  val COMMA_SET = CONSTANT_START_SET.clone().asInstanceOf[util.HashSet[PascalTokenType]]
  COMMA_SET.add(PascalTokenType.COMMA)
  COMMA_SET.add(PascalTokenType.COLON)
  COMMA_SET.addAll(StatementParser.STMT_START_SET)
  COMMA_SET.addAll(StatementParser.STMT_FOLLOW_SET)
}