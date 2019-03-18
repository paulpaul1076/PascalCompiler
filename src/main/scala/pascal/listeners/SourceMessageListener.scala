package pascal.listeners

import message.{Message, MessageListener, MessageType}

/**
 * Listener for source messages.
 */
class SourceMessageListener extends MessageListener {
  /**
   * Called to receive a message sent by a message producer.
   *
   * @param message message the message that was sent.
   */
  override def messageReceived(message: Message): Unit = {
    val body = message.body.asInstanceOf[List[Any]]

    message.messageType match {
      case MessageType.SOURCE_LINE =>
        val lineNumber = body(0).asInstanceOf[Int]
        val lineText = body(1).asInstanceOf[String]

        println(f"$lineNumber%03d $lineText%s")
    }
  }
}