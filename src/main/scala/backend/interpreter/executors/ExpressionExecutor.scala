package backend.interpreter.executors

import java.util

import backend.interpreter.{Executor, RuntimeErrorCode}
import intermediate.icodeimpl.{ICodeKeyImpl, ICodeNodeTypeImpl}
import intermediate.symtabimpl.SymTabKeyImpl
import intermediate.{ICodeNode, SymTabEntry}

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
        val entry = node.getAttribute(ICodeKeyImpl.ID).asInstanceOf[SymTabEntry]
        entry.getAttribute(SymTabKeyImpl.DATA_VALUE)
      case ICodeNodeTypeImpl.INTEGER_CONSTANT =>
        // Return the integer value.
        node.getAttribute(ICodeKeyImpl.VALUE).asInstanceOf[Int]
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
      // Must be a binary operator
      case _ =>
        executeBinaryOperator(node, nodeType)
    }
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

    val integerMode = operand1.isInstanceOf[Int] && operand2.isInstanceOf[Int]

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
    } else {
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