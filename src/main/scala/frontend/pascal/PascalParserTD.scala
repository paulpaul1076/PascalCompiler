package frontend.pascal

import frontend.{EofToken, Parser, Scanner, Token}
import message.{Message, MessageType}

/**
 * Top down pascal parser.
 *
 * @param scanner lexer for this parser.
 */
class PascalParserTD(scanner: Scanner) extends Parser(scanner) {
  override def parse(): Unit = {
    var token: Token = nextToken()
    val startTime = System.currentTimeMillis()

    while (!token.isInstanceOf[EofToken]) {
      token = nextToken()
    }

    val elapsedTime = (System.currentTimeMillis() - startTime) / 1000f
    sendMessage(new Message(MessageType.PARSER_SUMMARY, List(
      token.getLineNumber,
      getErrorCount,
      elapsedTime))
    )
  }

  /**
   * Return the number of syntax errors found by the parser.
   *
   * @return error count.
   */
  override def getErrorCount: Int = {
    0
  }
}
