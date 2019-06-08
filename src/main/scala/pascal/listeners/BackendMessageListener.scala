package pascal.listeners

import message.{Message, MessageListener, MessageType}
import pascal.Pascal
import ide.IDEControl._

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

//      case MessageType.SOURCE_LINE =>
//        if (Pascal.lines) {
//          val lineNumber = message.body.asInstanceOf[Int]
//          println(f">>> AT LINE $lineNumber%03d")
//        }
//      case MessageType.ASSIGN =>
//        if (Pascal.assign) {
//          val body: List[Any] = message.body.asInstanceOf[List[Any]]
//          val lineNumber = body(0).asInstanceOf[Int]
//          val variableName = body(1).asInstanceOf[String]
//          val value = body(2)
//
//          println(f">>> AT LINE $lineNumber%03d: $variableName = $value")
//        }
//
//      case MessageType.FETCH =>
//        if (Pascal.fetch) {
//          val body: List[Any] = message.body.asInstanceOf[List[Any]]
//          val lineNumber = body(0).asInstanceOf[Int]
//          val variableName = body(1).asInstanceOf[String]
//          val value = body(2)
//
//          println(f">>> AT LINE $lineNumber%03d: $variableName : $value")
//        }
//      case MessageType.CALL =>
//        if (Pascal.call) {
//          val body: List[Any] = message.body.asInstanceOf[List[Any]]
//          val lineNumber = body(0).asInstanceOf[Int]
//          val routineName = body(1).asInstanceOf[String]
//
//          println(f">>> AT LINE $lineNumber%03d: CALL $routineName")
//        }
//      case MessageType.RETURN =>
//        if (Pascal.`return`) {
//          val body: List[Any] = message.body.asInstanceOf[List[Any]]
//          val lineNumber = body(0).asInstanceOf[Int]
//          val routineName = body(1).asInstanceOf[String]
//
//          println(f">>> AT LINE $lineNumber%03d: RETURN FROM $routineName")
//        }

      case MessageType.INTERPRETER_SUMMARY =>
        val body = message.body.asInstanceOf[List[Any]]
        val executionCount = body(0).asInstanceOf[Int]
        val runtimeErrors = body(1).asInstanceOf[Int]
        val elapsedTime = body(2).asInstanceOf[Float]

        println(INTERPRETER_TAG + f"\n$executionCount%,20d statements executed." +
          f"\n$runtimeErrors%,20d runtime errors." +
          f"\n$elapsedTime%,20.2f seconds total execution time.\n")


      case MessageType.COMPILER_SUMMARY =>
        val body = message.body.asInstanceOf[List[Any]]
        val instructionCount = body(0).asInstanceOf[Int]
        val elapsedTime = body(1).asInstanceOf[Float]

        println(f"\n$instructionCount%,20d instruction generated." +
          f"\n$elapsedTime%,20.2f seconds total code generation time.\n")


//      case MessageType.ASSIGN =>
//        if (firstOutputMessage) {
//          println("\n===== OUTPUT =====\n")
//          firstOutputMessage = false
//        }
//        val body = message.body.asInstanceOf[List[Any]]
//        val lineNumber = body(0).asInstanceOf[Int]
//        val variableName = body(1).asInstanceOf[String]
//        val value = body(2)
//
//        println(f" >>> LINE $lineNumber%03d: $variableName%s = $value%s")
//
//
//      case MessageType.RUNTIME_ERROR =>
//        val body = message.body.asInstanceOf[List[Any]]
//        val errorMessage = body(0).asInstanceOf[String]
//        val lineNumber = body(1)
//
//        print("*** RUNTIME ERROR ")
//        if (lineNumber != null) {
//          print(f" AT LINE ${lineNumber.asInstanceOf[Int]}%03d")
//        }
//        println(s": $errorMessage")

      case _ =>
    }
  }

  private var firstOutputMessage = true
}