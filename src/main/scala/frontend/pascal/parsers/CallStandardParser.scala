package frontend.pascal.parsers

import frontend.pascal.{PascalErrorCode, PascalParserTD}
import frontend.{Parser, Token}
import intermediate.icodeimpl.{ICodeKeyImpl, ICodeNodeTypeImpl}
import intermediate.symtabimpl.{Predefined, RoutineCodeImpl, SymTabKeyImpl}
import intermediate.typeimpl.TypeFormImpl
import intermediate.{ICodeFactory, ICodeNode, RoutineCode, SymTabEntry}

class CallStandardParser(parent: PascalParserTD) extends CallParser(parent) {

  /**
    * Parser for predefined pascal functions.
    *
    * @param toket the initial token
    * @return the root of the generated parse tree.
    */
  override def parse(toket: Token): ICodeNode = {
    var curToken = toket
    val callNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.CALL)
    val pfId = Parser.symTabStack.lookup(curToken.getText.toLowerCase)
    val routineCode = pfId.getAttribute(SymTabKeyImpl.ROUTINE_CODE).asInstanceOf[RoutineCode]
    callNode.setAttribute(ICodeKeyImpl.ID, pfId)

    curToken = nextToken() // consume procedure or function identifier

    routineCode.asInstanceOf[RoutineCodeImpl] match {
      case RoutineCodeImpl.READ | RoutineCodeImpl.READLN => parseReadReadln(curToken, callNode, pfId)

      case RoutineCodeImpl.WRITE | RoutineCodeImpl.WRITELN => parseWriteWriteln(curToken, callNode, pfId)

      case RoutineCodeImpl.EOF | RoutineCodeImpl.EOLN => parseEofEoln(curToken, callNode, pfId)

      case RoutineCodeImpl.ABS | RoutineCodeImpl.SQR => parseAbsSqr(curToken, callNode, pfId)

      case RoutineCodeImpl.ARCTAN |
           RoutineCodeImpl.COS |
           RoutineCodeImpl.EXP |
           RoutineCodeImpl.LN |
           RoutineCodeImpl.SIN |
           RoutineCodeImpl.SQRT => parseArctanCosExpLnSinSqrt(curToken, callNode, pfId)

      case RoutineCodeImpl.PRED | RoutineCodeImpl.SUCC => parsePredSucc(curToken, callNode, pfId)

      case RoutineCodeImpl.CHR => parseChr(curToken, callNode, pfId)
      case RoutineCodeImpl.ODD => parseOdd(curToken, callNode, pfId)
      case RoutineCodeImpl.ORD => parseOrd(curToken, callNode, pfId)
      case RoutineCodeImpl.ROUND | RoutineCodeImpl.TRUNC => parseRoundTrunc(curToken, callNode, pfId)

      case _ => null // should never get here
    }
  }

  /**
    *
    * @param toket
    * @param callNode
    * @param pfId
    * @return
    */
  private def parseReadReadln(toket: Token, callNode: ICodeNode, pfId: SymTabEntry): ICodeNode = {
    // Parse any actual parameters.
    val parmsNode = parseActualParameters(toket, pfId, false, true, false)

    callNode.addChild(parmsNode)

    // Read must have parameters.
    if (pfId == Predefined.readId && callNode.getChildren.size() == 0) { // TODO: maybe parms node.getChildren???????
      PascalParserTD.errorHandler.flag(toket, PascalErrorCode.WRONG_NUMBER_OF_PARMS, this)
    }

    callNode
  }

  private def parseWriteWriteln(toket: Token, callNode: ICodeNode, pfId: SymTabEntry): ICodeNode = {
    // Parse any actual parameters.
    val parmsNode = parseActualParameters(toket, pfId, false, false, true)
    callNode.addChild(parmsNode)

    // Write must have parameters.
    if (pfId == Predefined.writeId && callNode.getChildren.size == 0) { // TODO: same as readln above
      PascalParserTD.errorHandler.flag(toket, PascalErrorCode.WRONG_NUMBER_OF_PARMS, this)
    }

    callNode
  }

  // TODO: What the heck is this even used for?
  private def parseEofEoln(toket: Token, callNode: ICodeNode, pfId: SymTabEntry): ICodeNode = {
    // Parse any actual parameters.
    val parmsNode = parseActualParameters(toket, pfId, false, false, false)
    callNode.addChild(parmsNode)

    // There should be no actual parameters.
    if (checkParmCount(toket, parmsNode, 0)) {
      callNode.setTypeSpec(Predefined.booleanType)
    }

    callNode
  }

  private def parseAbsSqr(toket: Token, callNode: ICodeNode, pfId: SymTabEntry): ICodeNode = {
    // Parse any actual parameters.
    val parmsNode = parseActualParameters(toket, pfId, false, false, false)
    callNode.addChild(parmsNode)

    // There should be one integer or real parameter.
    // The function return type is the parameter type.
    if (checkParmCount(toket, parmsNode, 1)) {
      val argType = parmsNode.getChildren.get(0).getTypeSpec.baseType
      if (argType == Predefined.integerType || argType == Predefined.realType) {
        callNode.setTypeSpec(argType)
      } else {
        PascalParserTD.errorHandler.flag(toket, PascalErrorCode.INVALID_TYPE, this)
      }
    }

    callNode
  }

  private def parseArctanCosExpLnSinSqrt(toket: Token, callNode: ICodeNode, pfId: SymTabEntry): ICodeNode = {
    // Parse any actual parameters.
    val parmsNode = parseActualParameters(toket, pfId, false, false, false)
    callNode.addChild(parmsNode)

    // There should be one integer or real parameter.
    // The function return type is real.
    if (checkParmCount(toket, parmsNode, 1)) {
      val argType = parmsNode.getChildren.get(0).getTypeSpec.baseType

      if (argType == Predefined.integerType || argType == Predefined.realType) {
        callNode.setTypeSpec(Predefined.realType)
      } else {
        PascalParserTD.errorHandler.flag(toket, PascalErrorCode.INVALID_TYPE, this)
      }
    }

    callNode
  }

  private def parsePredSucc(toket: Token, callNode: ICodeNode, pfId: SymTabEntry): ICodeNode = {
    // Parse any actual parameters.
    val parmsNode = parseActualParameters(toket, pfId, false, false, false)
    callNode.addChild(parmsNode)

    // There should be one integer or enumeration parameter.
    // The function return type is the parameter type.
    if (checkParmCount(toket, parmsNode, 1)) {
      val argType = parmsNode.getChildren.get(0).getTypeSpec.baseType
      if (argType == Predefined.integerType || argType.getForm == TypeFormImpl.ENUMERATION) {
        callNode.setTypeSpec(argType)
      } else {
        PascalParserTD.errorHandler.flag(toket, PascalErrorCode.INVALID_TYPE, this)
      }
    }

    callNode
  }

  private def parseChr(toket: Token, callNode: ICodeNode, pfId: SymTabEntry): ICodeNode = {
    // Parse any actual parameters.
    val parmsNode = parseActualParameters(toket, pfId, false, false, false)
    callNode.addChild(parmsNode)

    // There should be one integer parameter.
    // The function return type is character.
    if (checkParmCount(toket, parmsNode, 1)) {
      val argType = parmsNode.getChildren.get(0).getTypeSpec.baseType

      if (argType == Predefined.integerType) {
        callNode.setTypeSpec(Predefined.charType)
      } else {
        PascalParserTD.errorHandler.flag(toket, PascalErrorCode.INVALID_TYPE, this)
      }
    }

    callNode
  }

  private def parseOdd(toket: Token, callNode: ICodeNode, pfId: SymTabEntry): ICodeNode = {
    // Parse any actual parameters.
    val parmsNode = parseActualParameters(toket, pfId, false, false, false)
    callNode.addChild(parmsNode)

    // There should be one integer parameter.
    // The function return type is boolean.
    if (checkParmCount(toket, parmsNode, 1)) {
      val argType = parmsNode.getChildren.get(0).getTypeSpec.baseType
      if (argType == Predefined.integerType) {
        callNode.setTypeSpec(Predefined.booleanType)
      } else {
        PascalParserTD.errorHandler.flag(toket, PascalErrorCode.INVALID_TYPE, this)
      }
    }

    callNode
  }

  private def parseOrd(toket: Token, callNode: ICodeNode, pfId: SymTabEntry): ICodeNode = {
    // Parse any actual parameters.
    val parmsNode = parseActualParameters(toket, pfId, false, false, false)
    callNode.addChild(parmsNode)

    // There should be one character or enumeration parameter.
    // The function return type is integer.
    if (checkParmCount(toket, parmsNode, 1)) {
      val argType = parmsNode.getChildren.get(0).getTypeSpec.baseType
      if (argType == Predefined.charType || argType.getForm == TypeFormImpl.ENUMERATION) {
        callNode.setTypeSpec(Predefined.integerType)
      } else {
        PascalParserTD.errorHandler.flag(toket, PascalErrorCode.INVALID_TYPE, this)
      }
    }

    callNode
  }

  private def parseRoundTrunc(toket: Token, callNode: ICodeNode, pfId: SymTabEntry): ICodeNode = {
    // Parse any actual parameters.
    val parmsNode = parseActualParameters(toket, pfId, false, false, false)
    callNode.addChild(parmsNode)

    // There should be one real parameter.
    // The function return type is integer.
    if (checkParmCount(toket, parmsNode, 1)) {
      val argType = parmsNode.getChildren.get(0).getTypeSpec.baseType
      if (argType == Predefined.realType) {
        callNode.setTypeSpec(Predefined.integerType)
      } else {
        PascalParserTD.errorHandler.flag(toket, PascalErrorCode.INVALID_TYPE, this)
      }
    }

    callNode
  }

  private def checkParmCount(toket: Token, parmsNode: ICodeNode, count: Int): Boolean = {
    if (parmsNode == null && count == 0 || parmsNode.getChildren.size() == count) {
      return true
    } else {
      PascalParserTD.errorHandler.flag(toket, PascalErrorCode.WRONG_NUMBER_OF_PARMS, this)
      return false
    }
  }
}
