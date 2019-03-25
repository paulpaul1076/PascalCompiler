package backend.interpreter

import backend.Backend
import intermediate.ICodeNode
import intermediate.icodeimpl.ICodeKeyImpl
import message.{Message, MessageType}

/**
 * RuntimeErrorHandler.
 * This class is a delegate. It is delegated the responsibility of handling backend interpreter errors.
 */
class RuntimeErrorHandler {

  // TODO: too many responsibilities here. Separate them.
  /**
   * Flag a runtime error.
   *
   * @param node      the root node of the offending statement or expression.
   * @param errorCode the runtime error code.
   * @param backend   the backend processor.
   */
  def flag(node: ICodeNode, errorCode: RuntimeErrorCode, backend: Backend): Unit = {
    var lineNumber: String = null // TODO: remove this one?????
    var curNode = node

    // Look for the ancestor statement node with a line number attribute.
    while ((curNode != null) && (curNode.getAttribute(ICodeKeyImpl.LINE) == null)) {
      curNode = curNode.getParent
    }

    // Notify the interpreter's listeners.
    backend.sendMessage(new Message(
      MessageType.RUNTIME_ERROR,
      List[Any](errorCode.toString, curNode.getAttribute(ICodeKeyImpl.LINE))
    ))

    RuntimeErrorHandler.errorCount += 1
    if (RuntimeErrorHandler.errorCount > RuntimeErrorHandler.MAX_ERRORS) {
      println("*** ABORTED AFTER TOO MANY RUNTIME ERRORS.")
      System.exit(-1)
    }
  }

  /**
   * Getter.
   * @return number of errors.
   */
  def getErrorCount: Int = RuntimeErrorHandler.errorCount
}

private object RuntimeErrorHandler {
  val MAX_ERRORS = 5
  var errorCount = 0
}