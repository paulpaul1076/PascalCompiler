package pascal.listeners

import frontend.pascal.PascalTokenType
import message.{Message, MessageListener, MessageType}

/**
 * Companion object.
 */
private object ParserMessageListener {
  val PREFIX_WIDTH = 5
}

/**
 * Listener for parser messages.
 */
class ParserMessageListener extends MessageListener {
  /**
   * Called to receive a message sent by a message producer.
   *
   * @param message message the message that was sent.
   */
  override def messageReceived(message: Message): Unit = {
    message.messageType match {
      case MessageType.TOKEN =>
        val body = message.body.asInstanceOf[List[Any]]

        val lineNumber = body(0).asInstanceOf[Int]
        val position = body(1).asInstanceOf[Int]
        val tokenType = body(2).asInstanceOf[PascalTokenType]
        val tokenText = body(3).asInstanceOf[String]
        var tokenValue = body(4) // can be anything "Any"

        println(f">>> $tokenType%-15s line=$lineNumber%03d, pos=$position%2d, text=${'"'}$tokenText%s${'"'}")

        if (tokenValue != null) {
          if (tokenType == PascalTokenType.STRING) {
            tokenValue = "\"" + tokenValue + "\""
          }
          println(f">>>                 value=$tokenValue%s")
        }

      case MessageType.SYNTAX_ERROR   =>
        val body = message.body.asInstanceOf[List[Any]]

        val lineNumber = body(0).asInstanceOf[Int]
        val position = body(1).asInstanceOf[Int]
        val tokenText = body(2).asInstanceOf[String]
        val errorMessage = body(3).asInstanceOf[String]

        val spaceCount = ParserMessageListener.PREFIX_WIDTH + position
        val flagBuffer = new StringBuilder

        for (i <- 1 until spaceCount) {
          flagBuffer.append(' ')
        }

        flagBuffer.append("^\n*** ").append(errorMessage)

        if (tokenText != null) {
          flagBuffer.append(" [at \"").append(tokenText).append("\"]")
        }

        println(flagBuffer.toString())
      case MessageType.PARSER_SUMMARY =>
        val body = message.body.asInstanceOf[List[Any]]

        val statementCount = body(0).asInstanceOf[Int]
        val syntaxErrors = body(1).asInstanceOf[Int]
        val elapsedTime = body(2).asInstanceOf[Float]

        println(f"\n$statementCount%,20d source lines." +
                f"\n$syntaxErrors%,20d syntax errors." +
                f"\n$elapsedTime%,20.2f seconds total parsing time.\n")
    }
  }
}