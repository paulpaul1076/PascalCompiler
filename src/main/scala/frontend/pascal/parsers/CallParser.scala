package frontend.pascal.parsers

import java.util

import frontend.pascal.{PascalErrorCode, PascalParserTD, PascalTokenType}
import frontend.{Parser, Token}
import intermediate.icodeimpl.ICodeNodeTypeImpl
import intermediate.symtabimpl.{DefinitionImpl, Predefined, RoutineCodeImpl, SymTabKeyImpl}
import intermediate.typeimpl.{TypeChecker, TypeFormImpl}
import intermediate.{ICodeFactory, ICodeNode, RoutineCode, SymTabEntry}

/**
  * Main class for parsing function/procedure calls.
  *
  * @param parent parent parser for synchronization.
  */
class CallParser(parent: PascalParserTD) extends StatementParser(parent) {
  override def parse(token: Token): ICodeNode = {
    val pfId = Parser.symTabStack.lookup(token.getText.toLowerCase)
    val routineCode = pfId.getAttribute(SymTabKeyImpl.ROUTINE_CODE).asInstanceOf[RoutineCode]
    val callParser =
      if (routineCode == RoutineCodeImpl.DECLARED || routineCode == RoutineCodeImpl.FORWARD)
        new CallDeclaredParser(this)
      else
        new CallStandardParser(this)

    callParser.parse(token)
  }

  /**
    * Parse the actual parameters of a procedure or function call.
    *
    * @param toket          the current token
    * @param pfId           the symbol table entry of the procedure or function name.
    * @param isDeclared     true if parsing actual parms of a declared routine.
    * @param isReadReadln   true if parsing actual parms of a declared routine.
    * @param isWriteWriteln true if parsing actual parms of write or writeln.
    * @return the PARAMETERS node, or null if there are no actual parameters.
    */
  def parseActualParameters(toket: Token, pfId: SymTabEntry,
                            isDeclared: Boolean,
                            isReadReadln: Boolean,
                            isWriteWriteln: Boolean): ICodeNode = {
    var curToken = toket
    val expressionParser = new ExpressionParser(this)
    val parmsNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.PARAMETERS)
    var formalParms: util.ArrayList[SymTabEntry] = null

    var parmCount = 0
    var parmIndex = -1

    if (isDeclared) {
      formalParms = pfId.getAttribute(SymTabKeyImpl.ROUTINE_PARMS).asInstanceOf[util.ArrayList[SymTabEntry]]
      parmCount = if (formalParms != null) formalParms.size() else 0
    }

    if (curToken.getTokenType != PascalTokenType.LEFT_PAREN) {
      if (parmCount != 0) {
        PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.WRONG_NUMBER_OF_PARMS, this)
      }
      return null
    }

    curToken = nextToken() // consume opening (

    // Loop to parse each actual parameter.
    while (curToken.getTokenType != PascalTokenType.RIGHT_PAREN) {
      var actualNode = expressionParser.parse(curToken)

      // Declared procedure or function: Check the number of actual
      // parameters, and check each actual parameter against the
      // corresponding formal parameter.
      if (isDeclared) {
        parmIndex += 1
        if (parmIndex < parmCount) {
          val formalId = formalParms.get(parmIndex)
          checkActualParameter(curToken, formalId, actualNode)
        } else if (parmIndex == parmCount) {
          PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.WRONG_NUMBER_OF_PARMS, this)
        }
      }

      // read or readln: Each actual parameter must be a variable that is
      // a scalar, boolean, or subrange of integer
      else if (isReadReadln) {
        val `type` = actualNode.getTypeSpec
        val form = `type`.getForm

        if (actualNode.getType != ICodeNodeTypeImpl.VARIABLE &&
          (form == TypeFormImpl.SCALAR || `type` == Predefined.booleanType || form == TypeFormImpl.SUBRANGE && `type`.baseType == Predefined.integerType)) { // TODO: what about realType?
          PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.INVALID_VAR_PARM, this)
        }
      }

      // write or writeln: The type of each actual parameter must be a
      // scalar, boolean, or a Pascal string. Parse any field width and
      // precision.
      else if (isWriteWriteln) {
        // Create a WRITE_PARM node which adopts the expression node.
        val exprNode = actualNode
        actualNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.WRITE_PARM)
        actualNode.addChild(exprNode)

        val `type` = exprNode.getTypeSpec.baseType
        val form = `type`.getForm

        if (!(form == TypeFormImpl.SCALAR || `type` == Predefined.booleanType || `type`.isPascalString)) {
          PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.INCOMPATIBLE_TYPES, this)
        }

        // Optional field width.
        curToken = currentToken()
        actualNode.addChild(parseWriteSpec(curToken))

        // Optional precision.
        curToken = currentToken()
        actualNode.addChild(parseWriteSpec(curToken))
      }

      parmsNode.addChild(actualNode)
      curToken = synchronize(CallParser.COMMA_SET)
      val tokenType = curToken.getTokenType

      // Look for the comma.
      if (tokenType == PascalTokenType.COMMA) {
        curToken = nextToken() // consume ,
      } else if (ExpressionParser.EXPR_START_SET.contains(tokenType)) {
        PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_COMMA, this)
      } else if (tokenType != PascalTokenType.RIGHT_PAREN) {
        curToken = synchronize(ExpressionParser.EXPR_START_SET)
      }
    }

    curToken = nextToken() // consume closing )

    if (parmsNode.getChildren.size() == 0 || (isDeclared && (parmIndex != parmCount - 1))) {
      PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.WRONG_NUMBER_OF_PARMS, this)
    }

    parmsNode
  }

  def checkActualParameter(token: Token, formalId: SymTabEntry, actualNode: ICodeNode): Unit = {
    val formalDefn = formalId.getDefinition
    val formalType = formalId.getTypeSpec
    val actualType = actualNode.getTypeSpec

    // VAR parameter: The actual parameter must be a variable of the same type as the formal parameter.
    if (formalDefn == DefinitionImpl.VAR_PARM) {
      if (actualNode.getType != ICodeNodeTypeImpl.VARIABLE || actualType != formalType) {
        PascalParserTD.errorHandler.flag(token, PascalErrorCode.INVALID_VAR_PARM, this)
      }
    }
    // Value parameter: The actual parameter must be assignment-compatible with the formal parameter.
    else if (!TypeChecker.areAssignmentCompatible(formalType, actualType)) {
      PascalParserTD.errorHandler.flag(token, PascalErrorCode.INCOMPATIBLE_TYPES, this)
    }
  }

  def parseWriteSpec(toket: Token): ICodeNode = {
    var curToken = toket
    if (curToken.getTokenType == PascalTokenType.COLON) {
      curToken = nextToken() // consume :
      val expressionParser = new ExpressionParser(this)
      val specNode = expressionParser.parse(curToken)

      if (specNode.getType == ICodeNodeTypeImpl.INTEGER_CONSTANT) {
        return specNode
      } else {
        PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.INVALID_NUMBER, this)
        return null
      }
    } else {
      return null
    }
  }
}

object CallParser {
  val COMMA_SET = ExpressionParser.EXPR_START_SET.clone().asInstanceOf[util.HashSet[PascalTokenType]]
  COMMA_SET.add(PascalTokenType.COMMA)
  COMMA_SET.add(PascalTokenType.RIGHT_PAREN)
}