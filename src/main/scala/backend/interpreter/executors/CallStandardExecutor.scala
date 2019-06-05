package backend.interpreter.executors

import java.lang
import java.util.Locale

import backend.interpreter.{Executor, RuntimeErrorCode}
import frontend.pascal.PascalTokenType
import frontend.{Source, Token, TokenType}
import intermediate.icodeimpl.ICodeKeyImpl
import intermediate.symtabimpl.{Predefined, RoutineCodeImpl, SymTabKeyImpl}
import intermediate.{ICodeNode, RoutineCode, SymTabEntry, TypeSpec}

import scala.collection.JavaConverters._

/**
  * Execute a call to a standard procedure or function.
  *
  * @param parent parent executor.
  */
class CallStandardExecutor(parent: Executor) extends CallExecutor(parent) {
  var expressionExecutor: ExpressionExecutor = _

  /**
    * Execute a call to a standard procedure or function.
    *
    * @param node the root node of the statement.
    * @return null.
    */
  override def execute(node: ICodeNode): Any = {
    val routineId = node.getAttribute(ICodeKeyImpl.ID).asInstanceOf[SymTabEntry]
    val routineCode = routineId.getAttribute(SymTabKeyImpl.ROUTINE_CODE).asInstanceOf[RoutineCode]
    val `type` = node.getTypeSpec
    expressionExecutor = new ExpressionExecutor(this)
    var actualNode: ICodeNode = null

    // Get the actual parameters of the call.
    if (node.getChildren.size() > 0) {
      val parmsNode = node.getChildren.get(0)
      actualNode = parmsNode.getChildren.get(0)
    }

    return routineCode.asInstanceOf[RoutineCodeImpl] match {
      case RoutineCodeImpl.READ | RoutineCodeImpl.READLN => executeReadReadln(node, routineCode)
      case RoutineCodeImpl.WRITE | RoutineCodeImpl.WRITELN => executeWriteWriteln(node, routineCode)
      case RoutineCodeImpl.EOF | RoutineCodeImpl.EOLN => executeEofEoln(node, routineCode)
      case RoutineCodeImpl.ABS | RoutineCodeImpl.SQR => executeAbsSqr(node, routineCode, actualNode)
      case RoutineCodeImpl.ARCTAN | RoutineCodeImpl.COS | RoutineCodeImpl.EXP | RoutineCodeImpl.LN | RoutineCodeImpl.SIN | RoutineCodeImpl.SQRT => executeArctanCosExpLnSinSqrt(node, routineCode, actualNode)
      case RoutineCodeImpl.PRED | RoutineCodeImpl.SUCC => executePredSucc(node, routineCode, actualNode, `type`)
      case RoutineCodeImpl.CHR => executeChr(node, routineCode, actualNode)
      case RoutineCodeImpl.ODD => executeOdd(node, routineCode, actualNode)
      case RoutineCodeImpl.ORD => executeOrd(node, routineCode, actualNode)
      case RoutineCodeImpl.ROUND | RoutineCodeImpl.TRUNC => executeRoundTrunc(node, routineCode, actualNode)
      case _ => null // should never get here.
    }
  }

  private def executeReadReadln(callNode: ICodeNode, routineCode: RoutineCode): Any = {
    val parmsNode = if (callNode.getChildren.size() > 0) callNode.getChildren.get(0) else null

    if (parmsNode != null) {
      val actuals = parmsNode.getChildren

      // Loop to process each actual parameter.

      for (actualNode <- actuals.asScala) {
        val `type` = actualNode.getTypeSpec
        val baseType = `type`.baseType
        val variableCell = expressionExecutor.executeVariable(actualNode)
        var value: Any = null

        // Read a value of the appropriate type from the standard input.
        try {
          if (baseType == Predefined.integerType) {
            val token = Executor.standardIn.nextToken()
            value = parseNumber(token, baseType).asInstanceOf[Integer]
          } else if (baseType == Predefined.realType) {
            val token = Executor.standardIn.nextToken()
            value = parseNumber(token, baseType).asInstanceOf[Float]
          } else if (baseType == Predefined.booleanType) {
            val token = Executor.standardIn.nextToken()
            value = parseBoolean(token)
          } else if (baseType == Predefined.charType) {
            var ch = Executor.standardIn.nextChar()
            if (ch == Source.EOL || ch == Source.EOF) {
              ch = ' '
            }
            value = ch
          } else {
            throw new Exception
          }
        } catch {
          case ex: Exception =>
            Executor.errorHandler.flag(callNode, RuntimeErrorCode.INVALID_INPUT, CallStandardExecutor.this)

            if (`type` == Predefined.realType) {
              value = 0.0f
            } else if (`type` == Predefined.charType) {
              value = ' '
            } else if (`type` == Predefined.booleanType) {
              value = false
            } else {
              value = 0
            }

        }

        // Range check and set the value.
        value = checkRange(callNode, `type`, value)
        variableCell.setValue(value)

        val actualId = actualNode.getAttribute(ICodeKeyImpl.ID).asInstanceOf[SymTabEntry]
        sendAssignMessage(callNode, actualId.getName, value)
      }
    }

    // Skip the rest of the input line for readln.
    if (routineCode == RoutineCodeImpl.READLN) {
      try {
        Executor.standardIn.skipToNextLine
      } catch {
        case ex: Exception =>
          Executor.errorHandler.flag(callNode, RuntimeErrorCode.INVALID_INPUT, CallStandardExecutor.this)
      }
    }
    return null
  }

  private def parseNumber(toket: Token, `type`: TypeSpec): Number = {
    var curToken = toket
    var tokenType = curToken.getTokenType
    var sign: TokenType = null

    // Leading sign?
    if (tokenType == PascalTokenType.PLUS || tokenType == PascalTokenType.MINUS) {
      sign = tokenType
      curToken = Executor.standardIn.nextToken()
      tokenType = curToken.getTokenType
    }

    // Integer value.
    if (tokenType == PascalTokenType.INTEGER) {
      val value: Number = if (sign == PascalTokenType.MINUS) -curToken.getValue.asInstanceOf[Integer] else curToken.getValue.asInstanceOf[Integer]
      return if (`type` == Predefined.integerType) value else new java.lang.Float(value.asInstanceOf[Integer].intValue())
    }
    // Real value.
    else if (tokenType == PascalTokenType.REAL) {
      val value: Number = if (sign == PascalTokenType.MINUS) -curToken.getValue.asInstanceOf[java.lang.Float] else curToken.getValue.asInstanceOf[java.lang.Float]
      return if (`type` == Predefined.realType) value else new Integer(value.asInstanceOf[java.lang.Float].intValue())
    }
    // Bad input.
    else {
      throw new Exception
    }
  }

  private def parseBoolean(token: Token): Boolean = {
    if (token.getTokenType == PascalTokenType.IDENTIFIER) {
      val text = token.getText

      if (text.equalsIgnoreCase("true")) {
        return new java.lang.Boolean(true)
      } else if (text.equalsIgnoreCase("false")) {
        return new lang.Boolean(false)
      } else {
        throw new Exception
      }
    } else {
      throw new Exception
    }
  }

  private def executeWriteWriteln(callNode: ICodeNode, routineCode: RoutineCode): Any = {
    val parmsNode = if (callNode.getChildren.size() > 0) callNode.getChildren.get(0) else null
    if (parmsNode != null) {
      val actuals = parmsNode.getChildren

      // Loop to process each WRITE_PARM actual parameter node.
      for (writeParmNode <- actuals.asScala) {
        val children = writeParmNode.getChildren
        val exprNode = children.get(0)
        val dataType = exprNode.getTypeSpec.baseType
        val typeCode =
          if (dataType.isPascalString) "s"
          else if (dataType == Predefined.integerType) "d"
          else if (dataType == Predefined.realType) "f"
          else if (dataType == Predefined.booleanType) "s"
          else if (dataType == Predefined.charType) "c"
          else "s"

        var value = expressionExecutor.execute(exprNode)
        if (dataType == Predefined.charType && value.isInstanceOf[String]) {
          value = value.asInstanceOf[String].charAt(0)
        }

        // Java format string.
        val format = new StringBuilder("%")

        // Process any field width and precision values.
        if (children.size() > 1) {
          val w = children.get(1).getAttribute(ICodeKeyImpl.VALUE).asInstanceOf[Integer]
          format.append(if (w == 0) 1 else w)
        }
        if (children.size() > 2) {
          val p = children.get(2).getAttribute(ICodeKeyImpl.VALUE).asInstanceOf[Integer]
          format.append(".")
          format.append(if (p == 0) 1 else p)
        }

        format.append(typeCode)

        // Write the formatted value to the standard output.
        Executor.standardOut.printf(format.toString(), value.asInstanceOf[AnyRef])
        Executor.standardOut.flush()
      }
    }

    // Line feed for writeln.
    if (routineCode == RoutineCodeImpl.WRITELN) {
      Executor.standardOut.println()
      Executor.standardOut.flush()
    }

    return null
  }

  private def executeEofEoln(callNode: ICodeNode, routineCode: RoutineCode): Boolean = {
    try {
      if (routineCode == RoutineCodeImpl.EOF) {
        return Executor.standardIn.atEof()
      } else {
        return Executor.standardIn.atEol()
      }
    } catch {
      case ex: Exception =>
        Executor.errorHandler.flag(callNode, RuntimeErrorCode.INVALID_INPUT, CallStandardExecutor.this)
        return true
    }
  }

  private def executeAbsSqr(callNode: ICodeNode, routineCode: RoutineCode, actualNode: ICodeNode): Number = {
    val argValue = expressionExecutor.execute(actualNode)

    if (argValue.isInstanceOf[Integer]) {
      val value = argValue.asInstanceOf[Integer]
      return if (routineCode == RoutineCodeImpl.ABS) Math.abs(value) else value * value
    } else {
      val value = argValue.asInstanceOf[java.lang.Float]
      return if (routineCode == RoutineCodeImpl.ABS) Math.abs(value) else value * value
    }
  }

  private def executeArctanCosExpLnSinSqrt(callNdde: ICodeNode, routineCode: RoutineCode, actualNode: ICodeNode): java.lang.Float = {
    val argValue = expressionExecutor.execute(actualNode)
    val value: java.lang.Double = if (argValue.isInstanceOf[Integer]) argValue.asInstanceOf[Integer].doubleValue() else argValue.asInstanceOf[java.lang.Double]

    routineCode.asInstanceOf[RoutineCodeImpl] match {
      case RoutineCodeImpl.ARCTAN => Math.atan(value).asInstanceOf[java.lang.Float]
      case RoutineCodeImpl.COS => Math.cos(value).asInstanceOf[java.lang.Float]
      case RoutineCodeImpl.EXP => Math.exp(value).asInstanceOf[java.lang.Float]
      case RoutineCodeImpl.SIN => Math.sin(value).asInstanceOf[java.lang.Float]
      case RoutineCodeImpl.LN =>
        if (value > 0.0f) {
          Math.log(value).asInstanceOf[java.lang.Float]
        } else {
          Executor.errorHandler.flag(callNdde, RuntimeErrorCode.INVALID_STANDARD_FUNCTION_ARGUMENT, CallStandardExecutor.this)
          0.0f
        }
      case RoutineCodeImpl.SQRT =>
        if (value >= 0.0f) {
          Math.sqrt(value).asInstanceOf[java.lang.Float]
        } else {
          Executor.errorHandler.flag(callNdde, RuntimeErrorCode.INVALID_STANDARD_FUNCTION_ARGUMENT, CallStandardExecutor.this)
          0.0f
        }
      case _ => 0.0f // should never get here
    }
  }

  private def executePredSucc(callNode: ICodeNode, routineCode: RoutineCode, actualNode: ICodeNode, `type`: TypeSpec): Integer = {
    val value = expressionExecutor.execute(actualNode).asInstanceOf[Integer]
    var newValue = if (routineCode == RoutineCodeImpl.PRED) value - 1 else value + 1

    newValue = checkRange(callNode, `type`, newValue).asInstanceOf[Integer]
    newValue
  }

  private def executeChr(callNode: ICodeNode, routineCode: RoutineCode, actualNode: ICodeNode): Character = {
    val value = expressionExecutor.execute(actualNode).asInstanceOf[Integer]
    val ch = value.asInstanceOf[Character]
    ch
  }

  private def executeOdd(callNode: ICodeNode, routineCode: RoutineCode, actualNode: ICodeNode): Boolean = {
    val value = expressionExecutor.execute(actualNode).asInstanceOf[Integer]
    value % 1 == 1
  }

  private def executeOrd(callNode: ICodeNode, routineCode: RoutineCode, actualNode: ICodeNode): Integer = {
    val value = expressionExecutor.execute(actualNode)

    if (value.isInstanceOf[Character]) {
      val ch = value.asInstanceOf[Character].charValue()
      return ch.asInstanceOf[Int]
    } else if (value.isInstanceOf[String]) {
      val ch = value.asInstanceOf[String].charAt(0)
      return ch.asInstanceOf[Int]
    } else {
      return value.asInstanceOf[Int]
    }
  }

  private def executeRoundTrunc(callNode: ICodeNode, routineCode: RoutineCode, actualNode: ICodeNode): Integer = {
    val value = expressionExecutor.execute(actualNode).asInstanceOf[java.lang.Float]

    if (routineCode == RoutineCodeImpl.ROUND) {
      return if (value >= 0.0f) (value + 0.5f).asInstanceOf[Int] else (value - 0.5f).asInstanceOf[Int]
    } else {
      return value.asInstanceOf[Int]
    }
  }
}
