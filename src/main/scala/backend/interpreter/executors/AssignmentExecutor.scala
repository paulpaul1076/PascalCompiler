package backend.interpreter.executors

import backend.interpreter.Executor
import intermediate.{ICodeNode, SymTabEntry}
import intermediate.icodeimpl.ICodeKeyImpl
import intermediate.symtabimpl.SymTabKeyImpl
import message.{Message, MessageType}

/**
 * Assignment executor.
 *
 * @param parent parent executor.
 */
class AssignmentExecutor(parent: Executor) extends StatementExecutor(parent) {

  /**
   * Execute an assignment statement.
   *
   * @param node the root node of the statement.
   * @return null.
   */
  override def execute(node: ICodeNode): Any = {
    // The ASSIGN node's children are the target variable
    // and the expression.
    val children = node.getChildren
    val variableNode = children.get(0)
    val expressionNode = children.get(1)

    // Execute the expression and get its value.
    val expressionExecutor = new ExpressionExecutor(this)
    val value = expressionExecutor.execute(expressionNode)

    // Set the value as an attribute of the variable's symbol table entry.
    val variableId = variableNode.getAttribute(ICodeKeyImpl.ID).asInstanceOf[SymTabEntry]
    variableId.setAttribute(SymTabKeyImpl.DATA_VALUE, value)
    sendMessage(node, variableId.getName, value)

    Executor.executionCount += 1
    null
  }

  /**
   * Send a message about the assignment operation.
   *
   * @param node         the assign node.
   * @param variableName the name of the target variable.
   * @param value        the value of the expression.
   */
  private def sendMessage(node: ICodeNode, variableName: String, value: Any): Unit = {
    val lineNumber = node.getAttribute(ICodeKeyImpl.LINE)

    // Send an ASSIGN message.
    if (lineNumber != null) {
      sendMessage(new Message(
        MessageType.ASSIGN, List[Any](lineNumber, variableName, value)
      ))
    }
  }
}
