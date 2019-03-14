package frontend.pascal

import java.io.IOException

import frontend.{EofToken, Parser, Scanner, Token}
import message.{Message, MessageType}

/**
 * Top down pascal parser.
 *
 * @param scanner lexer for this parser.
 */
class PascalParserTD(scanner: Scanner) extends Parser(scanner) {

  /**
   * Fetches the next token by means of scanner.
   * For each token it sends a token message to every subscribed listener.
   */
  override def parse(): Unit = {

    try {
      var token = nextToken()
      val startTime = System.currentTimeMillis()

      while (!token.isInstanceOf[EofToken]) {
        val tokenType = token.getTokenType
        if (tokenType != PascalTokenType.ERROR) {
          sendMessage(new Message(
            MessageType.TOKEN,
            List(
              token.getLineNumber,
              token.getPosition,
              tokenType,
              token.getText,
              token.getValue
            )
          ))
        } else {
          PascalParserTD.errorHandler.flag(token, token.getValue.asInstanceOf[PascalErrorCode], this)
        }

        token = nextToken()
      }

      val elapsedTime = (System.currentTimeMillis() - startTime) / 1000f
      sendMessage(new Message(MessageType.PARSER_SUMMARY, List[Any](
        token.getLineNumber,
        getErrorCount,
        elapsedTime))
      )
    } catch {
      case ex: IOException => PascalParserTD.errorHandler.abortTranslation(PascalErrorCode.IO_ERROR, this)
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

  protected object PascalParserTD {
    val errorHandler = new PascalErrorHandler
  }
}
