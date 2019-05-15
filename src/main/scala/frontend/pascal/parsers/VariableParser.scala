package frontend.pascal.parsers

import java.util

import frontend.pascal.{PascalErrorCode, PascalParserTD, PascalTokenType}
import frontend.{Parser, Token}
import intermediate._
import intermediate.icodeimpl.{ICodeKeyImpl, ICodeNodeTypeImpl}
import intermediate.symtabimpl.{DefinitionImpl, Predefined}
import intermediate.typeimpl.{TypeChecker, TypeFormImpl, TypeKeyImpl}

/**
  * Parses variables into ICodeNodes.
  *
  * @param parent parent parser.
  */
class VariableParser(parent: PascalParserTD) extends StatementParser(parent) {

  /**
    * Parse a variable.
    *
    * @param toket the initial token.
    * @return the root node of the generated parse tree.
    */
  override def parse(toket: Token): ICodeNode = {
    // Look up the identifier in the symbol table stack.
    val name = toket.getText.toLowerCase
    var variableId = Parser.symTabStack.lookup(name)

    //If not found, flag the error and enter the identifier
    // as an undefined identifier with an undefined type.
    if (variableId == null) {
      PascalParserTD.errorHandler.flag(toket, PascalErrorCode.IDENTIFIER_UNDEFINED, this)
      variableId = Parser.symTabStack.enterLocal(name)
      variableId.setDefinition(DefinitionImpl.UNDEFINED)
      variableId.setTypeSpec(Predefined.undefinedType)
    }

    parse(toket, variableId)
  }

  // Set to true to parse a function name.
  // as the target of an assignment.
  private var isFunctionTarget = false

  private def parseSubscripts(variableTyp: TypeSpec): ICodeNode = {
    var variableType = variableTyp
    var curToken: Token = null
    val expressionParser = new ExpressionParser(this)

    // Create a SUBSCRIPTS node.
    val subscriptsNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.SUBSCRIPTS)

    do {
      curToken = nextToken() // consume the [ or , token

      //The current variable is an array.
      if (variableType.getForm == TypeFormImpl.ARRAY) {
        // Parse the subscript expression.
        val exprNode = expressionParser.parse(curToken)
        val exprType = if (exprNode != null) exprNode.getTypeSpec else Predefined.undefinedType

        // The subscript expression type must be assignment.
        // compatible with the array index type.
        val indexType = variableType.getAttribute(TypeKeyImpl.ARRAY_INDEX_TYPE).asInstanceOf[TypeSpec]
        if (!TypeChecker.areAssignmentCompatible(indexType, exprType)) {
          PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.INCOMPATIBLE_TYPES, this)
        }

        // The SUBSCRIPTS node adopts the subscript expression tree.
        subscriptsNode.addChild(exprNode)

        // Update the variable's type.
        variableType = variableType.getAttribute(TypeKeyImpl.ARRAY_ELEMENT_TYPE).asInstanceOf[TypeSpec]
      } // Not an array type, so too many subscripts.
      else {
        PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.TOO_MANY_SUBSCRIPTS, this)
        expressionParser.parse(curToken)
      }
      curToken = currentToken()
    } while (curToken.getTokenType == PascalTokenType.COMMA)

    // Synchronize at the ] token.
    curToken = synchronize(VariableParser.RIGHT_BRACKET_SET)
    if (curToken.getTokenType == PascalTokenType.RIGHT_BRACKET) {
      curToken = nextToken() // consume the ] token
    } else {
      PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_RIGHT_BRACKET, this)
    }

    subscriptsNode.setTypeSpec(variableType)
    subscriptsNode
  }

  private def parseField(variableTyp: TypeSpec): ICodeNode = {
    var variableType = variableTyp
    // Create FIELD node.
    val fieldNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.FIELD)

    var curToken = nextToken() // consume the . token
    val tokenType = curToken.getTokenType
    val variableForm = variableType.getForm

    if (tokenType == PascalTokenType.IDENTIFIER && variableForm == TypeFormImpl.RECORD) {
      val symTab = variableType.getAttribute(TypeKeyImpl.RECORD_SYMTAB).asInstanceOf[SymTab]
      val fieldName = curToken.getText.toLowerCase
      val fieldId = symTab.lookup(fieldName)

      if (fieldId != null) {
        variableType = fieldId.getTypeSpec
        fieldId.appendLineNumber(curToken.getLineNumber)

        // Set the field identifier's name.
        fieldNode.setAttribute(ICodeKeyImpl.ID, fieldId)
      } else {
        PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.INVALID_FIELD, this)
      }
    } else {
      PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.INVALID_FIELD, this)
    }
    curToken = nextToken() // consume the field identifier.

    fieldNode.setTypeSpec(variableType)
    fieldNode
  }

  /**
    * Parse a variable.
    *
    * @param toket      the initial token.
    * @param variableId the symbol table entry of the variable identifier.
    * @return the root node of the generated parse tree.
    */
  def parse(toket: Token, variableId: SymTabEntry): ICodeNode = {

    var curToken = toket
    // Check how the variable is defined.
    val defnCode = variableId.getDefinition
    if ((defnCode != DefinitionImpl.VARIABLE) &&
      (defnCode != DefinitionImpl.VALUE_PARM) &&
      (defnCode != DefinitionImpl.VAR_PARM) &&
      (!isFunctionTarget || defnCode != DefinitionImpl.FUNCTION)) {

      PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.INVALID_IDENTIFIER_USAGE, this)
    }

    variableId.appendLineNumber(curToken.getLineNumber)

    val variableNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.VARIABLE)
    variableNode.setAttribute(ICodeKeyImpl.ID, variableId)

    curToken = nextToken() // consume the identifier

    // Parse array subscripts or record fields.
    var variableType = variableId.getTypeSpec

    if (!isFunctionTarget) { // function can't be followed by subscripts or ".fields".
      while (VariableParser.SUBSCRIPT_FIELD_START_SET.contains(curToken.getTokenType)) {
        val subFldNode = if (curToken.getTokenType == PascalTokenType.LEFT_BRACKET) parseSubscripts(variableType) else parseField(variableType)
        curToken = currentToken()

        // Update the variable's type.
        // The variable node adopts the SUBSCRIPTS or FIELD node.
        variableType = subFldNode.getTypeSpec
        variableNode.addChild(subFldNode)
      }
    }

    variableNode.setTypeSpec(variableType)
    variableNode
  }

  /**
    * Parse a function name as the target of an assignment statement.
    *
    * @param toket the initial token.
    * @return the root node of the generated parse tree.
    */
  def parseFunctionNameTarget(toket: Token): ICodeNode = {
    isFunctionTarget = true
    parse(toket)
  }
}

object VariableParser {
  val SUBSCRIPT_FIELD_START_SET = new util.HashSet[PascalTokenType]()
  SUBSCRIPT_FIELD_START_SET.add(PascalTokenType.LEFT_BRACKET)
  SUBSCRIPT_FIELD_START_SET.add(PascalTokenType.DOT)

  val RIGHT_BRACKET_SET = new util.HashSet[PascalTokenType]
  RIGHT_BRACKET_SET.add(PascalTokenType.RIGHT_BRACKET)
  RIGHT_BRACKET_SET.add(PascalTokenType.EQUALS)
  RIGHT_BRACKET_SET.add(PascalTokenType.SEMICOLON)
}
