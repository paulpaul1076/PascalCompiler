package frontend.pascal

import java.io.IOException
import java.util

import frontend.pascal.parsers.StatementParser
import frontend.{EofToken, Parser, Scanner, Token}
import intermediate.{ICodeFactory, ICodeNode}
import message.{Message, MessageType}

/**
  * Top down pascal parser.
  *
  * @param scanner lexer for this parser.
  */
class PascalParserTD(scanner: Scanner) extends Parser(scanner) {

  def this(pascalParser: PascalParserTD) {
    this(pascalParser.getScanner)
  }

  /**
    * Fetches the next token by means of scanner.
    * For each token it sends a token message to every subscribed listener.
    */
  override def parse(): Unit = {

    try {
      var token = nextToken()
      val startTime = System.currentTimeMillis()
      iCode = ICodeFactory.createICode()
      var rootNode: ICodeNode = null

      //while (!token.isInstanceOf[EofToken]) {

      // Look for the begin token then parse a compound statement.
      if (token.getTokenType == PascalTokenType.BEGIN) {
        val statementParser = new StatementParser(this)
        rootNode = statementParser.parse(token)
        token = currentToken()
      } else {
        PascalParserTD.errorHandler.flag(token, PascalErrorCode.UNEXPECTED_TOKEN, this)
      }
      // Look for the final period.
      if (token.getTokenType != PascalTokenType.DOT) {
        PascalParserTD.errorHandler.flag(token, PascalErrorCode.MISSING_PERIOD, this)
      }
      token = currentToken()
      // Set the parse tree root node.
      if (rootNode != null) {
        iCode.setRoot(rootNode)
      }

      val elapsedTime = (System.currentTimeMillis() - startTime) / 1000f
      sendMessage(new Message(MessageType.PARSER_SUMMARY, List[Any](
        token.getLineNumber,
        getErrorCount,
        elapsedTime))
      )
    } catch {
      case _: IOException => PascalParserTD.errorHandler.abortTranslation(PascalErrorCode.IO_ERROR, this)
    }
  }

  /**
    * Return the number of syntax errors found by the parser.
    *
    * @return error count.
    */
  override def getErrorCount: Int = {
    PascalParserTD.errorHandler.getErrorCount
  }

  /**
    * Synchronize the parser.
    *
    * @param syncSet the set of token types for synchronizing the parser.
    * @return the token where the parser has synchronized.
    */
  def synchronize(syncSet: util.EnumSet[PascalTokenType]): Token = {
    var token = currentToken()

    // If the current token is not in the synchronization set,
    // then it is unexpected and the parser must recover.
    if (!syncSet.contains(token.getTokenType)) {
      // Flag the unexpected token.
      PascalParserTD.errorHandler.flag(token, PascalErrorCode.UNEXPECTED_TOKEN, this)

      // Recover by skipping tokens that are not
      // in the synchronization set.
      do {
        token = nextToken()
      } while (!token.isInstanceOf[EofToken] && !syncSet.contains(token.getTokenType))
    }
    token
  }

  protected object PascalParserTD {
    val errorHandler = new PascalErrorHandler
  }

}
