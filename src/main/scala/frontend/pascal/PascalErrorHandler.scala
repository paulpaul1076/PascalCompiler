package frontend.pascal

import frontend.{Parser, Token}
import message.{Message, MessageType}

/**
 * Error handler for pascal syntax errors.
 */
class PascalErrorHandler {

  /**
   * Flag an error in the source line.
   *
   * @param token the bad token.
   * @param errorCode the error code.
   * @param parser the parser
   */
  def flag(token: Token, errorCode: PascalErrorCode, parser: Parser): Unit = {
    parser.sendMessage(new Message(
      MessageType.SYNTAX_ERROR,
      List[Any](
        token.getLineNumber,
        token.getPosition,
        token.getText,
        errorCode.toString
      ))
    )
    PascalErrorHandler.errorCount += 1
    if (PascalErrorHandler.errorCount > PascalErrorHandler.MAX_ERRORS) {
      abortTranslation(PascalErrorCode.TOO_MANY_ERRORS, parser)
    }
  }

  /**
   * Abort the translation.
   *
   * @param errorCode the error code.
   * @param parser the parser.
   */
  def abortTranslation(errorCode: PascalErrorCode, parser: Parser): Unit = {
    val fatalText = "FATAL ERROR: " + errorCode.toString
    parser.sendMessage(new Message(MessageType.SYNTAX_ERROR,
      List[Any](
        0,
        0,
        "",
        fatalText
      ))
    )
  }

  def getErrorCount : Int = PascalErrorHandler.errorCount
}

private object PascalErrorHandler {
  val MAX_ERRORS = 25
  var errorCount = 0
}