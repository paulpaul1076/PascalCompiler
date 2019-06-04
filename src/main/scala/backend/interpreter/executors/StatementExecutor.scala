package backend.interpreter.executors


import java.util

import backend.interpreter.{Cell, Executor, MemoryFactory, RuntimeErrorCode}
import intermediate.icodeimpl.{ICodeKeyImpl, ICodeNodeTypeImpl}
import intermediate.symtabimpl.Predefined
import intermediate.typeimpl.{TypeFormImpl, TypeKeyImpl}
import intermediate.{ICodeNode, TypeSpec}
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
      case ICodeNodeTypeImpl.CALL =>
        val callExecutor = new CallExecutor(this)
        callExecutor.execute(node)
      case ICodeNodeTypeImpl.COMPOUND =>
        val compoundExecutor = new CompoundExecutor(this)
        compoundExecutor.execute(node)
      case ICodeNodeTypeImpl.ASSIGN   =>
        val assignmentExecutor = new AssignmentExecutor(this)
        assignmentExecutor.execute(node)
      case ICodeNodeTypeImpl.LOOP =>
        val loopExecutor = new LoopExecutor(this)
        loopExecutor.execute(node)
      case ICodeNodeTypeImpl.IF =>
        val ifExecutor = new IfExecutor(this)
        ifExecutor.execute(node)
      case ICodeNodeTypeImpl.SELECT =>
        val selectExecutor = new SelectExecutorFast(this)
        selectExecutor.execute(node)
      case ICodeNodeTypeImpl.NO_OP    =>
        null
      case _                          =>
        Executor.errorHandler.flag(node, RuntimeErrorCode.UNIMPLEMENTED_FEATURE, this)
        null
    }
  }

  /**
    * Convert a Java string to a Pascal string or character.
    *
    * @param targetType the target type specification.
    * @param javaValue  the Java string.
    * @return the Pascal string or character.
    */
  def toPascal(targetType: TypeSpec, javaValue: Any): Any = {
    if (javaValue.isInstanceOf[String]) {
      val string = javaValue.asInstanceOf[String]
      if (targetType == Predefined.charType) {
        return string.charAt(0) //Not a cell, because it's later on put into a cell, in AssignmentExecutor, for example.
      } else if (targetType.isPascalString) {
        val charCells = new Array[Cell](string.length)

        // Build an array of characters.
        for (i <- 0 until string.length) {
          charCells(i) = MemoryFactory.createCell(string.charAt(i))
        }

        return charCells // Pascal string (array of characters
      } else {
        return javaValue
      }
    } else {
      return javaValue
    }
  }

  /**
    * Convert a Pascal string to a Java string.
    *
    * @param targetType  the target type specification.
    * @param pascalValue the Pascal string.
    * @return the Java string.
    */
  def toJava(targetType: TypeSpec, pascalValue: Any): Any = {
    if (pascalValue.isInstanceOf[Array[Cell]] && (pascalValue.asInstanceOf[Array[Cell]]) (0).getValue.isInstanceOf[Character]) {
      val charCells = pascalValue.asInstanceOf[Array[Cell]]
      val string = new StringBuilder(charCells.length)

      // Bui.d a Java string.
      for (ref <- charCells) {
        string.append(ref.getValue.asInstanceOf[Character])
      }

      return string.toString() // Java string
    } else {
      return pascalValue
    }
  }

  /**
    * Return a copy of a Pascal value.
    *
    * @param value the value.
    * @param node  the statement node.
    * @return the copy.
    */
  def copyOf(value: Any, node: ICodeNode): Any = {
    var copy: Any = null

    if (value.isInstanceOf[Integer]) {
      copy = new Integer(value.asInstanceOf[Integer])
    } else if (value.isInstanceOf[Float]) {
      copy = new java.lang.Float(value.asInstanceOf[java.lang.Float])
    } else if (value.isInstanceOf[Character]) {
      copy = new Character(value.asInstanceOf[java.lang.Character])
    } else if (value.isInstanceOf[Boolean]) {
      copy = new java.lang.Boolean(value.asInstanceOf[java.lang.Boolean])
    } else if (value.isInstanceOf[String]) {
      copy = new String(value.asInstanceOf[String])
    } else if (value.isInstanceOf[util.HashMap[String, Any]]) {
      copy = copyRecord(value.asInstanceOf[util.HashMap[String, Any]], node)
    } else {
      copy = copyArray(value.asInstanceOf[Array[Cell]], node)
    }

    return copy
  }

  /**
    * Runtime range check.
    *
    * @param node   the root node of the expression subtree to check.
    * @param `type` the target type specification.
    * @param value  the value.
    * @return the value to use.
    */
  def checkRange(node: ICodeNode, `type`: TypeSpec, value: Any): Any = {
    if (`type`.getForm == TypeFormImpl.SUBRANGE) {
      val minValue = `type`.getAttribute(TypeKeyImpl.SUBRANGE_MIN_VALUE).asInstanceOf[Integer]
      val maxValue = `type`.getAttribute(TypeKeyImpl.SUBRANGE_MAX_VALUE).asInstanceOf[Integer]

      if (value.asInstanceOf[Integer] < minValue) {
        Executor.errorHandler.flag(node, RuntimeErrorCode.VALUE_RANGE, this)
        return minValue
      } else if (value.asInstanceOf[Integer] > maxValue) {
        Executor.errorHandler.flag(node, RuntimeErrorCode.VALUE_RANGE, this)
        return maxValue
      } else {
        return value
      }
    } else {
      return value
    }
  }

  def sendAssignMessage(node: ICodeNode, variableName: String, value: Any): Unit = {
    val lineNumber = getLineNumber(node)

    // Send an ASSIGN message.
    if (lineNumber != null) {
      sendMessage(new Message(MessageType.ASSIGN, List[Any](lineNumber, variableName, value)))
    }
  }

  /**
    * Sends a message when a value is fetched from a memory map.
    *
    * @param node
    * @param variableName
    * @param value
    */
  def sendFetchMessage(node: ICodeNode, variableName: String, value: Any): Unit = {
    val lineNumber = getLineNumber(node)

    // Send a FETCH meesage.
    if (lineNumber != null) {
      sendMessage(new Message(MessageType.FETCH, List[Any](lineNumber, variableName, value)))
    }
  }

  //---------------------------------------------
  //---------------------------------------------
  //---------------------------------------------
  //---------------------------------------------
  //---------------------------------------------
  //               DEBUGGING
  //---------------------------------------------
  //---------------------------------------------
  //---------------------------------------------
  //---------------------------------------------
  //---------------------------------------------

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

  def getLineNumber(node: ICodeNode): Any = {
    var lineNumber: Any = null
    var curNode = node
    // Go up the parent links to look for a line number.
    lineNumber = node.getAttribute(ICodeKeyImpl.LINE)
    while (curNode != null && lineNumber == null) {
      curNode = curNode.getParent
      lineNumber = node.getAttribute(ICodeKeyImpl.LINE)
    }

    return lineNumber
  }

  def sendCallMessage(node: ICodeNode, routineName: String): Unit = {
    val lineNumber = getLineNumber(node)

    // Send a CALL messsage.
    if (lineNumber != null) {
      sendMessage(new Message(MessageType.CALL, List[Any](lineNumber, routineName)))
    }
  }

  def sendReturnMessage(node: ICodeNode, routineName: String): Unit = {
    val lineNumber = getLineNumber(node)

    // Send a RETURN messsage.
    if (lineNumber != null) {
      sendMessage(new Message(MessageType.RETURN, List[Any](lineNumber, routineName)))
    }
  }

  /**
    * Return a copy of a pascal record.
    *
    * @param value the record value hashmap.
    * @param node  the statement node.
    * @return the copy of the hashmap
    */
  private def copyRecord(value: util.HashMap[String, Any], node: ICodeNode): Any = {
    val copy = new util.HashMap[String, Any]()

    if (value != null) {
      val entries = value.entrySet()
      val it = entries.iterator()

      while (it.hasNext) {
        val entry = it.next()
        val newKey = new String(entry.getKey)
        val valueCell = entry.getValue.asInstanceOf[Cell]
        val newValue = copyOf(valueCell.getValue, node)

        copy.put(newKey, MemoryFactory.createCell(newValue))
      }
    } else {
      Executor.errorHandler.flag(node, RuntimeErrorCode.UNINITIALIZED_VALUE, this)
    }

    return copy
  }

  /**
    * Return a copy of a Pascal array.
    *
    * @param valueCells the array cells.
    * @param node       the statement node
    * @return the copy of the array cells.
    */
  private def copyArray(valueCells: Array[Cell], node: ICodeNode): Array[Cell] = {
    var length: Int = 0
    var copy: Array[Cell] = null

    if (valueCells != null) {
      length = valueCells.length
      copy = new Array[Cell](length)

      for (i <- 0 until length) {
        val valueCell = valueCells(i)
        val newValue = copyOf(valueCell.getValue, node)
        copy(i) = MemoryFactory.createCell(newValue)
      }
    } else {
      Executor.errorHandler.flag(node, RuntimeErrorCode.UNINITIALIZED_VALUE, this)
      copy = new Array[Cell](1)
    }

    return copy
  }
}
