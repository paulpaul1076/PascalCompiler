package backend.interpreter.executors

import java.util

import backend.BackendFactory
import backend.interpreter.{Cell, Executor, RuntimeErrorCode}
import intermediate.icodeimpl.{ICodeKeyImpl, ICodeNodeTypeImpl}
import intermediate.symtabimpl.{Predefined, RoutineCodeImpl, SymTabKeyImpl}
import intermediate.typeimpl.{TypeFormImpl, TypeKeyImpl}
import intermediate.{ICodeNode, RoutineCode, SymTabEntry, TypeSpec}

import scala.collection.JavaConverters._

/**
  * Expression executor.
  *
  * @param parent parent executor.
  */
class ExpressionExecutor(parent: Executor) extends StatementExecutor(parent) {

  /**
    * Execute a node.
    *
    * @param node the root node of the statement.
    * @return null.
    */
  override def execute(node: ICodeNode): Any = {
    val nodeType = node.getType.asInstanceOf[ICodeNodeTypeImpl]
    nodeType match {
      case ICodeNodeTypeImpl.VARIABLE =>
        // Get variable's symbol table entry and return its value.
        //        val entry = node.getAttribute(ICodeKeyImpl.ID).asInstanceOf[SymTabEntry]
        //        entry.getAttribute(SymTabKeyImpl.DATA_VALUE)

        executeValue(node)
      case ICodeNodeTypeImpl.INTEGER_CONSTANT =>
        // Return the integer value.
        //        node.getAttribute(ICodeKeyImpl.VALUE).asInstanceOf[Int]

        val `type` = node.getTypeSpec
        val value = node.getAttribute(ICodeKeyImpl.VALUE).asInstanceOf[Integer]

        // If boolean, return true or false.
        // Else return the integer value
        if (`type` == Predefined.booleanType)
          value == 1
        else
          value
      case ICodeNodeTypeImpl.REAL_CONSTANT =>
        // Return the float value.
        node.getAttribute(ICodeKeyImpl.VALUE).asInstanceOf[Float]
      case ICodeNodeTypeImpl.STRING_CONSTANT =>
        // Return the string value.
        node.getAttribute(ICodeKeyImpl.VALUE).asInstanceOf[String]
      case ICodeNodeTypeImpl.NEGATE =>
        // Get the NEGATE node's expression node child.
        val children = node.getChildren
        val expressionNode = children.get(0)

        // Execute the expression and return the negative of its value.
        val value = execute(expressionNode)

        // TODO: does this work?
        //  do I need to write 'asInstanceOf'?
        if (value.isInstanceOf[Int]) {
          -value.asInstanceOf[Int]
        } else {
          -value.asInstanceOf[Float]
        }
      case ICodeNodeTypeImpl.NOT =>
        // Get the NOT node's expression node child.
        val children = node.getChildren
        val expressionNode = children.get(0)

        // Execute the expression and return the "not" of its value.
        val value = execute(expressionNode).asInstanceOf[Boolean]
        !value

      case ICodeNodeTypeImpl.CALL =>
        // Execute a function call.
        val functionId = node.getAttribute(ICodeKeyImpl.ID).asInstanceOf[SymTabEntry]
        val routineCode = functionId.getAttribute(SymTabKeyImpl.ROUTINE_CODE).asInstanceOf[RoutineCode]
        val callExecutor = new CallExecutor(this)
        var value = callExecutor.execute(node)

        // If it was a declared function, obtain the function value.
        // from its name.
        if (routineCode == RoutineCodeImpl.DECLARED) {
          val functionName = functionId.getName
          val nestingLevel = functionId.getSymTab.getNestingLevel
          val ar = Executor.runtimeStack.getTopmost(nestingLevel)
          val functionValueCell = ar.getCell(functionName)
          value = functionValueCell.getValue

          sendFetchMessage(node, functionId.getName, value)
        }

        value
      // Must be a binary operator
      case _ =>
        executeBinaryOperator(node, nodeType)
    }
  }

  /**
    * Return a variable's value.
    * @param node ICodeNode.
    * @return Any value.
    */
  def executeValue(node: ICodeNode): Any = {
    val variableId = node.getAttribute(ICodeKeyImpl.ID).asInstanceOf[SymTabEntry]
    val variableName = variableId.getName
    val variableType = variableId.getTypeSpec

    // Get the variable's value
    val variableCell = executeVariable(node)
    var value = variableCell.getValue

    if (value != null) {
      value = toJava(variableType, value)
    }
    // Uninitialized value error: Use a default value.
    else {
      Executor.errorHandler.flag(node, RuntimeErrorCode.UNINITIALIZED_VALUE, this)

      value = BackendFactory.defaultValue(variableType)
      variableCell.setValue(value)
    }

    sendFetchMessage(node, variableName, value)
    value
  }

  /**
    * Execute a variable and return a reference to it's cell.
    * @param node the variable node.
    * @return the reference to the variable's cell.
    */
  def executeVariable(node: ICodeNode): Cell = {
    val variableId = node.getAttribute(ICodeKeyImpl.ID).asInstanceOf[SymTabEntry]
    val variableName = variableId.getName
    var variableType = variableId.getTypeSpec
    val nestingLevel = variableId.getSymTab.getNestingLevel

    // Get the variable reference from the appropriate activation record.
    val ar = Executor.runtimeStack.getTopmost(nestingLevel)
    var variableCell = ar.getCell(variableName)

    val modifiers = node.getChildren

    // Reference to a reference: Use the original reference.
    if (variableCell.getValue.isInstanceOf[Cell]) {
      variableCell = variableCell.getValue.asInstanceOf[Cell]
    }

    // Execute any array subscripts or record fields.
    for (modifier <- modifiers.asScala) {
      val nodeType = modifier.getType

      // Subscripts.
      if (nodeType == ICodeNodeTypeImpl.SUBSCRIPTS) {
        val subscripts = modifier.getChildren

        // Compute a new reference for each subscript.
        for (subscript <- subscripts.asScala) {
          val indexType = variableType.getAttribute(TypeKeyImpl.ARRAY_INDEX_TYPE).asInstanceOf[TypeSpec]
          val minIndex: Integer = if (indexType.getForm == TypeFormImpl.SUBRANGE) indexType.getAttribute(TypeKeyImpl.SUBRANGE_MIN_VALUE).asInstanceOf[Integer] else 0

          var value = execute(subscript).asInstanceOf[Integer]

          value = checkRange(node, indexType, value).asInstanceOf[Integer]

          val index = value - minIndex
          variableCell = variableCell.getValue.asInstanceOf[Array[Cell]](index)
          variableType = variableType.getAttribute(TypeKeyImpl.ARRAY_ELEMENT_TYPE).asInstanceOf[TypeSpec]
        }
      }
      // Field.
      else if (nodeType == ICodeNodeTypeImpl.FIELD) {
        val fieldId = modifier.getAttribute(ICodeKeyImpl.ID).asInstanceOf[SymTabEntry]
        val fieldName = fieldId.getName

        // Compute a new reference for the field.
        val map = variableCell.getValue.asInstanceOf[util.HashMap[String, Cell]]
        variableCell = map.get(fieldName)
        variableType = fieldId.getTypeSpec
      }
    }
    return variableCell
  }

  /**
    * Execute the binary operator.
    *
    * @param node     operator node.
    * @param nodeType type of operator node.
    * @return the result.
    */
  def executeBinaryOperator(node: ICodeNode, nodeType: ICodeNodeTypeImpl): Any = {
    // Get the two operand children of the operator node.
    val children = node.getChildren
    val operandNode1 = children.get(0)
    val operandNode2 = children.get(1)

    // Operands
    val operand1 = execute(operandNode1)
    val operand2 = execute(operandNode2)

    var integerMode = false
    var characterMode = false
    var stringMode = false

    if (operand1.isInstanceOf[Integer] && operand2.isInstanceOf[Integer]) {
      integerMode = true
    } else if ((operand1.isInstanceOf[Character] || operand1.isInstanceOf[String] && operand1.asInstanceOf[String].length == 1) &&
      (operand2.isInstanceOf[Character] || operand2.isInstanceOf[String] && operand2.asInstanceOf[String].length == 1)) {
      characterMode = true
    } else if (operand1.isInstanceOf[String] && operand2.isInstanceOf[String]) {
      stringMode = true
    }

    // ====================
    // Arithmetic operators
    // ====================

    if (ExpressionExecutor.ARITH_OPS.contains(nodeType)) {
      if (integerMode) {
        val value1 = operand1.asInstanceOf[Int]
        val value2 = operand2.asInstanceOf[Int]

        // Integer operations.
        return nodeType match {
          case ICodeNodeTypeImpl.ADD => value1 + value2
          case ICodeNodeTypeImpl.SUBTRACT => value1 - value2
          case ICodeNodeTypeImpl.MULTIPLY => value1 * value2
          case ICodeNodeTypeImpl.FLOAT_DIVIDE =>
            // Check for division by zero.
            if (value2 != 0) {
              value1.asInstanceOf[Float] / value2.asInstanceOf[Float]
            } else {
              Executor.errorHandler.flag(node, RuntimeErrorCode.DIVISION_BY_ZERO, this)
              0
            }
          case ICodeNodeTypeImpl.INTEGER_DIVIDE =>
            // Check for division by zero.
            if (value2 != 0) {
              value1 / value2
            } else {
              Executor.errorHandler.flag(node, RuntimeErrorCode.DIVISION_BY_ZERO, this)
              0
            }
          case ICodeNodeTypeImpl.MOD =>
            // Check for division by zero.
            if (value2 != 0) {
              value1 % value2
            } else {
              Executor.errorHandler.flag(node, RuntimeErrorCode.DIVISION_BY_ZERO, this)
              0
            }
        }
      } else {
        val value1 = if (operand1.isInstanceOf[Int]) operand1.asInstanceOf[Int] else operand1.asInstanceOf[Float]
        val value2 = if (operand2.isInstanceOf[Int]) operand2.asInstanceOf[Int] else operand2.asInstanceOf[Float]

        // Float operations.
        return nodeType match {
          case ICodeNodeTypeImpl.ADD => value1 + value2
          case ICodeNodeTypeImpl.SUBTRACT => value1 - value2
          case ICodeNodeTypeImpl.MULTIPLY => value1 * value2
          case ICodeNodeTypeImpl.FLOAT_DIVIDE =>
            // Check for division by zero.
            if (value2 != 0.0f) {
              value1 / value2
            } else {
              Executor.errorHandler.flag(node, RuntimeErrorCode.DIVISION_BY_ZERO, this)
              0.0f
            }
        }
      }
    }
    //===========
    // AND and OR
    //===========
    else if ((nodeType == ICodeNodeTypeImpl.AND) || (nodeType == ICodeNodeTypeImpl.OR)) {
      val value1 = operand1.asInstanceOf[Boolean]
      val value2 = operand2.asInstanceOf[Boolean]

      nodeType match {
        case ICodeNodeTypeImpl.AND => return value1 && value2
        case ICodeNodeTypeImpl.OR => return value1 || value2
      }
    }
    // TODO: do we have to differentiate between float and int in this case?
    //=====================
    // Relational operators
    //=====================
    else if (integerMode) {
      val value1 = operand1.asInstanceOf[Int]
      val value2 = operand2.asInstanceOf[Int]

      // Integer operands.
      return nodeType match {
        case ICodeNodeTypeImpl.EQ => value1 == value2
        case ICodeNodeTypeImpl.NE => value1 != value2
        case ICodeNodeTypeImpl.LT => value1 < value2
        case ICodeNodeTypeImpl.LE => value1 <= value2
        case ICodeNodeTypeImpl.GT => value1 > value2
        case ICodeNodeTypeImpl.GE => value1 >= value2
      }
    }
    else if (characterMode) {
      val value1: Character = if (operand1.isInstanceOf[Character]) operand1.asInstanceOf[Character] else operand1.asInstanceOf[String].charAt(0)
      val value2: Character = if (operand2.isInstanceOf[Character]) operand2.asInstanceOf[Character] else operand2.asInstanceOf[String].charAt(0)

      // Character operands
      return nodeType match {
        case ICodeNodeTypeImpl.EQ =>
          value1 == value2
        case ICodeNodeTypeImpl.NE =>
          value1 != value2
        case ICodeNodeTypeImpl.LT =>
          value1 < value2
        case ICodeNodeTypeImpl.LE =>
          value1 <= value2
        case ICodeNodeTypeImpl.GT =>
          value1 > value2
        case ICodeNodeTypeImpl.GE =>
          value1 >= value2
      }
    }
    else if (stringMode) {
      val value1 = operand1.asInstanceOf[String]
      val value2 = operand2.asInstanceOf[String]

      // String operands
      val comp = value1.compareTo(value2)
      return nodeType match {
        case ICodeNodeTypeImpl.EQ =>
          comp == 0
        case ICodeNodeTypeImpl.NE =>
          comp != 0
        case ICodeNodeTypeImpl.LT =>
          comp < 0
        case ICodeNodeTypeImpl.LE =>
          comp <= 0
        case ICodeNodeTypeImpl.GT =>
          comp > 0
        case ICodeNodeTypeImpl.GE =>
          comp >= 0
      }
    }
    else {
      val value1 = if (operand1.isInstanceOf[Int]) operand1.asInstanceOf[Int] else operand1.asInstanceOf[Float]
      val value2 = if (operand2.isInstanceOf[Int]) operand2.asInstanceOf[Int] else operand2.asInstanceOf[Float]

      // Float operands.
      return nodeType match {
        case ICodeNodeTypeImpl.EQ => value1 == value2
        case ICodeNodeTypeImpl.NE => value1 != value2
        case ICodeNodeTypeImpl.LT => value1 < value2
        case ICodeNodeTypeImpl.LE => value1 <= value2
        case ICodeNodeTypeImpl.GT => value1 > value2
        case ICodeNodeTypeImpl.GE => value1 >= value2
      }
    }
    0 // should never get here
  }
}

/**
  * Companion object for the above class.
  */
private object ExpressionExecutor {
  /**
    * Set of arithmetic operator node types.
    */
  val ARITH_OPS = new util.HashSet[ICodeNodeTypeImpl]
  ARITH_OPS.add(ICodeNodeTypeImpl.ADD)
  ARITH_OPS.add(ICodeNodeTypeImpl.SUBTRACT)
  ARITH_OPS.add(ICodeNodeTypeImpl.MULTIPLY)
  ARITH_OPS.add(ICodeNodeTypeImpl.FLOAT_DIVIDE)
  ARITH_OPS.add(ICodeNodeTypeImpl.INTEGER_DIVIDE)
}