package pascal.listeners

import message.{Message, MessageListener, MessageType}

/**
 * Listener for backend messages.
 */
class BackendMessageListener extends MessageListener {
  /**
   * Called to receive a message sent by a message producer.
   *
   * @param message the message that was sent.
   */
  override def messageReceived(message: Message): Unit = {
    message.messageType match {
      case MessageType.INTERPRETER_SUMMARY =>
        val body = message.body.asInstanceOf[List[Any]]
        val executionCount = body(0).asInstanceOf[Int]
        val runtimeErrors = body(1).asInstanceOf[Int]
        val elapsedTime = body(2).asInstanceOf[Float]

        println(f"\n$executionCount%,20d statements executed." +
          f"\n$runtimeErrors%,20d runtime errors." +
          f"\n$elapsedTime%,20.2f seconds total execution time.\n")

      case MessageType.COMPILER_SUMMARY =>
        val body = message.body.asInstanceOf[List[Any]]
        val instructionCount = body(0).asInstanceOf[Int]
        val elapsedTime = body(1).asInstanceOf[Float]

        println(f"\n$instructionCount%,20d instruction generated." +
          f"\n$elapsedTime%,20.2f seconds total code generation time.\n")
    }
  }
}