package backend.interpreter.executors


import backend.interpreter.{Executor, RuntimeErrorCode}
import intermediate.ICodeNode
import intermediate.icodeimpl.{ICodeKeyImpl, ICodeNodeTypeImpl}
import message.{Message, MessageType}

/**
 * Class that executes statements.
 *
 * @param parent parent executor.
 */
class StatementExecutor(parent: Executor) extends Executor(parent) {

  /**
   * Execute a statement.
   * To be overridden by the specialized statement executor subclasses.
   *
   * @param node the root node of the statement.
   * @return null.
   */
  def execute(node: ICodeNode): Any = {
    val nodeType = node.getType.asInstanceOf[ICodeNodeTypeImpl]

    // Send message about the current source line.
    sendSourceLineMessage(node)

    nodeType match {
      case ICodeNodeTypeImpl.COMPOUND =>
        val compoundExecutor = new CompoundExecutor(this)
        compoundExecutor.execute(node)
      case ICodeNodeTypeImpl.ASSIGN   =>
        val assignmentExecutor = new AssignmentExecutor(this)
        assignmentExecutor.execute(node)
      case ICodeNodeTypeImpl.NO_OP    =>
        null
      case _                          =>
        Executor.errorHandler.flag(node, RuntimeErrorCode.UNIMPLEMENTED_FEATURE, this)
        null
    }
  }

  /**
   * Send a message about the current source line.
   *
   * @param node the statement node.
   */
  private def sendSourceLineMessage(node: ICodeNode): Unit = {
    val lineNumber: Any = node.getAttribute(ICodeKeyImpl.LINE)

    // Send the SOURCE_LINE message.
    if (lineNumber != null) {
      sendMessage(new Message(MessageType.SOURCE_LINE, lineNumber))
    }
  }
}
