package pascal

import message.{Message, MessageListener, MessageType}
import java.io.{BufferedReader, FileReader}

import backend.{Backend, BackendFactory}
import frontend.{FrontendFactory, Parser, Source, TokenType}
import intermediate.{ICode, SymTab}

/**
 * Compile or interpret a Pascal source program.
 */
class Pascal(operation: String, filePath: String, flags: String) {

  private var parser: Parser = _
  private var source: Source = _
  private var iCode: ICode = _
  private var symTab: SymTab = _
  private var backend: Backend = _

  try {
    val intermediate = flags.indexOf('i') > -1
    val xref = flags.indexOf('x') > -1

    source = new Source(new BufferedReader(new FileReader(filePath)))
    source.addMessageListener(new SourceMessageListener())

    parser = FrontendFactory.createParser("Pascal", "top-down", source)
    parser.addMessageListener(new ParserMessageListener())

    backend = BackendFactory.createBackend(operation)
    backend.addMessageListener(new BackendMessageListener())

    parser.parse()
    source.close()

    iCode = parser.getICode
    symTab = parser.getSymTab

    backend.process(iCode, symTab)
  } catch {
    case e: Exception => println("*****Internal translator error. *****")
      e.printStackTrace()
  }

  /**
   * Listener for source messages.
   */
  private class SourceMessageListener extends MessageListener {
    /**
     * Called to receive a message sent by a message producer.
     *
     * @param message message the message that was sent.
     */
    override def messageReceived(message: Message): Unit = {
      val body = message.body.asInstanceOf[List[Any]]

      message.messageType match {
        case MessageType.SOURCE_LINE =>
          val lineNumber = body(0).asInstanceOf[Integer]
          val lineText = body(1).asInstanceOf[String]
          println(String.format(Pascal.SOURCE_LINE_FORMAT, lineNumber, lineText))

      }
    }
  }

  /**
   * Listener for parser messages.
   */
  private class ParserMessageListener extends MessageListener {
    /**
     * Called to receive a message sent by a message producer.
     *
     * @param message message the message that was sent.
     */
    override def messageReceived(message: Message): Unit = {
      message.messageType match {
        case MessageType.TOKEN               =>
          val body = message.body.asInstanceOf[List[Any]]

          val lineNumber = body(0).asInstanceOf[Int]
          val position = body(1).asInstanceOf[Int]
          val tokenType = body(2).asInstanceOf[TokenType]
          val tokenText = body(3).asInstanceOf[String]
          val tokenValue = body(4) // can be anything "Any"

          println(s"\ntoken's line number is $lineNumber" +
            s"\nposition is $position" +
            s"\ntype is $tokenType" +
            s"\ntext is $tokenText" +
            s"\nvalue is $tokenValue"
          )
        case MessageType.SYNTAX_ERROR =>
          val body = message.body.asInstanceOf[List[Any]]

          val lineNumber = body(0).asInstanceOf[Int]
          val position = body(1).asInstanceOf[Int]
          val tokenText = body(2).asInstanceOf[String]
          val errorMessage = body(3).asInstanceOf[String]

          val spaceCount = PREFIX_WIDTH + position
          val flagBuffer = new StringBuilder

          1 until spaceCount foreach flagBuffer.append(' ')

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

          //println(String.format(Pascal.PARSER_SUMMARY_FORMAT, statementCount, syntaxErrors, elapsedTime))
          println(s"\n$statementCount source lines." +
            s"\n$syntaxErrors syntax errors." +
            s"\n$elapsedTime seconds total parsing time.\n")
      }
    }
  }

  /**
   * Listener for backend messages.
   */
  private class BackendMessageListener extends MessageListener {
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

          //println(String.format(Pascal.INTERPRETER_SUMMARY_FORMAT, executionCount, runtimeErrors, elapsedTime))
          println(s"\n$executionCount statements executed." +
            s"\n$runtimeErrors runtime errors." +
            s"\n$elapsedTime seconds total execution time.\n")

        case MessageType.COMPILER_SUMMARY =>
          val body = message.body.asInstanceOf[List[Any]]
          val instructionCount = body(0).asInstanceOf[Int]
          val elapsedTime = body(1).asInstanceOf[Float]

          //println(String.format(Pascal.COMPILER_SUMMARY_FORMAT, instructionCount, elapsedTime))
          println(s"\n$instructionCount instruction generated." +
            s"\n$elapsedTime seconds total code generation time.\n")
      }
    }
  }

}

/**
 * Companion object.
 */
object Pascal {
  val FLAGS = "[-ix]"
  val USAGE = "Usage: Pascal execute|compile " + FLAGS + " <source file path>"

  val SOURCE_LINE_FORMAT = "%03d %s"
  val PARSER_SUMMARY_FORMAT = "" +
    "\n%,20d source lines." +
    "\n%,20d syntax errors." +
    "\n%,20.2f seconds total parsing time.\n"
  val INTERPRETER_SUMMARY_FORMAT =
    "\n%,20d statements executed." +
      "\n%,20d runtime errors." +
      "\n%20.2f seconds total execution time.\n"
  val COMPILER_SUMMARY_FORMAT =
    "\n%,20d instruction generated." +
      "\n%,20.2f seconds total code generation time.\n"

  /**
   * Program's entry point.
   *
   * @param args cmd args.
   */
  def main(args: Array[String]): Unit = {
    try {
      val operation = args(0)
      if (!operation.equalsIgnoreCase("compile") &&
        !operation.equalsIgnoreCase("execute")) {
        throw new Exception
      }

      var i = 1
      var flags = ""

      while (i < args.length && args(i).charAt(0) == '-') {
        flags += args(i).substring(1)
        i += 1
      }

      if (i < args.length) {
        val path = args(i)
        new Pascal(operation, path, flags)
      } else {
        throw new Exception
      }
    } catch {
      case _: Exception => println(Pascal.USAGE)
    }
  }
}