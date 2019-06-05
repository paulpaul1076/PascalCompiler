package backend.interpreter.executors

import backend.interpreter.{Cell, Executor}
import intermediate.icodeimpl.ICodeKeyImpl
import intermediate.symtabimpl.Predefined
import intermediate.typeimpl.TypeKeyImpl
import intermediate.{ICodeNode, SymTabEntry, TypeSpec}
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
    val variableId = variableNode.getAttribute(ICodeKeyImpl.ID).asInstanceOf[SymTabEntry]

    // Execute the expression and get its reference and
    // execute the expression to get its value.
    val expressionExecutor = new ExpressionExecutor(this)
    val targetCell = expressionExecutor.executeVariable(variableNode)
    val targetType = variableNode.getTypeSpec
    val valueType = expressionNode.getTypeSpec.baseType
    val value = expressionExecutor.execute(expressionNode)

    assignValue(node, variableId, targetCell, targetType, value, valueType)

    Executor.executionCount += 1
    null
  }

  def assignValue(node: ICodeNode, targetId: SymTabEntry,
                  targetCell: Cell, targetType: TypeSpec,
                  valu: Any, valueType: TypeSpec): Unit = {
    // Range check
    var x = valu
    x = checkRange(node, targetType, x)

    // Set the target's value
    // Convert an integer value to real if necessary.
    if (targetType == Predefined.realType && valueType == Predefined.integerType) {
      targetCell.setValue(new java.lang.Float(x.asInstanceOf[Integer].intValue()))
    }
    // String assignment:
    //    target length < value length: truncate the value
    //    target length > value length: blank pad the value
    else if (targetType.isPascalString) {
      val targetLength = targetType.getAttribute(TypeKeyImpl.ARRAY_ELEMENT_COUNT).asInstanceOf[Int]
      val valueLength = valueType.getAttribute(TypeKeyImpl.ARRAY_ELEMENT_COUNT).asInstanceOf[Int]
      var stringValue = x.asInstanceOf[String]

      // Truncate the value string.
      if (targetLength < valueLength) {
        stringValue = stringValue.substring(0, targetLength)
      }
      // Pad the value string with blanks at the right end.
      else if (targetLength > valueLength) {
        val buffer = new StringBuilder(stringValue)

        for (i <- valueLength until targetLength) {
          buffer.append(" ")
        }

        stringValue = buffer.toString()
      }

      targetCell.setValue(copyOf(toPascal(targetType, stringValue), node))
    }
    // Simple assignment.
    else {
      targetCell.setValue(copyOf(toPascal(targetType, x), node))
    }

    sendAssignMessage(node, targetId.getName, x)
  }
}
