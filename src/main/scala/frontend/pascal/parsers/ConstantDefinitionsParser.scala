package frontend.pascal.parsers

import java.util

import frontend.pascal.{PascalErrorCode, PascalParserTD, PascalTokenType}
import frontend.{Parser, Token, TokenType}
import intermediate.symtabimpl.{DefinitionImpl, Predefined, SymTabKeyImpl}
import intermediate.{TypeFactory, TypeSpec}

/**
  * Parses th CONST declaration block.
  *
  * @param parent parent parser to pick up where we left off.
  */
class ConstantDefinitionsParser(parent: PascalParserTD) extends PascalParserTD(parent) {

  /**
    * Parse method.
    *
    * @param toket start token.
    */
  def parse(toket: Token): Unit = {
    var curToken = synchronize(ConstantDefinitionsParser.IDENTIFIER_SET)

    // Loop to parse a sequence of constant definitions.
    // separated by semicolons.
    while (curToken.getTokenType == PascalTokenType.IDENTIFIER) {
      val name = curToken.getText.toLowerCase
      var constantId = Parser.symTabStack.lookupLocal(name)

      // Enter the new identifier into the symbol table.
      // but don't set how it's defined yet.
      if (constantId == null) {
        constantId = Parser.symTabStack.enterLocal(name)
        constantId.appendLineNumber(curToken.getLineNumber)
      } else {
        PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.IDENTIFIER_REDEFINED, this)
        constantId = null
      }

      curToken = nextToken() // consume the identifier token

      // Synchronize on the = token
      curToken = synchronize(ConstantDefinitionsParser.EQUALS_SET)
      if (curToken.getTokenType == PascalTokenType.EQUALS) {
        curToken = nextToken() // consume the =
      } else {
        PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_EQUALS, this)
      }
      // Parse the constant value.
      val constantToken = curToken
      val value = parseConstant(curToken)

      // Set identifier to be a constant and set its value.
      if (constantId != null) {
        constantId.setDefinition(DefinitionImpl.CONSTANT)
        constantId.setAttribute(SymTabKeyImpl.CONSTANT_VALUE, value)

        // Set the constant's type.
        val constantType =
          if (constantToken.getTokenType == PascalTokenType.IDENTIFIER)
            getConstantType(constantToken)
          else
            getConstantType(value)

        constantId.setTypeSpec(constantType)
      }

      curToken = currentToken()
      val tokenType = curToken.getTokenType

      // Look for one or more semicolons after a definitions.
      if (tokenType == PascalTokenType.SEMICOLON) {
        while (curToken.getTokenType == PascalTokenType.SEMICOLON) {
          curToken = nextToken() // consume the ;
        }
      }

      // If at the start of the next definition or declaration,
      // then missing a semicolon.
      else if (ConstantDefinitionsParser.NEXT_START_SET.contains(tokenType)) {
        PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_SEMICOLON, this)
      }

      curToken = synchronize(ConstantDefinitionsParser.IDENTIFIER_SET)
    }
  }

  // TODO: Shouldn't it parse expressions?
  /**
    * Parses the RHS of the constant definition.
    *
    * @param toket starting token.
    * @return object.
    */
  def parseConstant(toket: Token): Any = {
    var sign: TokenType = null

    // Synchronize at the start of a constant
    var curToken = synchronize(ConstantDefinitionsParser.CONSTANT_START_SET)
    val tokenType = curToken.getTokenType

    // Plus or minus sign?
    if ((tokenType == PascalTokenType.PLUS) || (tokenType == PascalTokenType.MINUS)) {
      sign = tokenType
      curToken = nextToken() // consume sign
    }

    // Parse the constant.
    curToken.getTokenType.asInstanceOf[PascalTokenType] match {
      case PascalTokenType.IDENTIFIER => parseIdentifierConstant(curToken, sign)
      case PascalTokenType.INTEGER =>
        val value = curToken.getValue.asInstanceOf[Int]
        nextToken() // consume the number
        if (sign == PascalTokenType.MINUS) -value else value
      case PascalTokenType.REAL =>
        val value = curToken.getValue.asInstanceOf[Float]
        nextToken() // consume the number
        if (sign == PascalTokenType.MINUS) -value else value
      case PascalTokenType.STRING =>
        if (sign != null) {
          PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.INVALID_CONSTANT, this)
        }
        nextToken() // consume the string
        curToken.getValue.asInstanceOf[String]
      case _ =>
        PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.INVALID_CONSTANT, this)
        null
    }
  }

  def parseIdentifierConstant(token: Token, sign: TokenType): Any = {
    val name = token.getText.toLowerCase
    val id = Parser.symTabStack.lookup(name)

    nextToken() // consume the identifier

    // The identifier must have already been defined
    // as a constant identifier.
    if (id == null) {
      PascalParserTD.errorHandler.flag(token, PascalErrorCode.IDENTIFIER_UNDEFINED, this)
      return null
    }
    val definition = id.getDefinition
    if (definition == DefinitionImpl.CONSTANT) {
      val value = id.getAttribute(SymTabKeyImpl.CONSTANT_VALUE)
      id.appendLineNumber(token.getLineNumber)

      if (value.isInstanceOf[Int]) {
        return if (sign == PascalTokenType.MINUS) -value.asInstanceOf[Int] else value
      } else if (value.isInstanceOf[Float]) {
        return if (sign == PascalTokenType.MINUS) -value.asInstanceOf[Float] else value
      } else if (value.isInstanceOf[String]) {
        if (sign != null) {
          PascalParserTD.errorHandler.flag(token, PascalErrorCode.INVALID_CONSTANT, this)
        }
        return value
      } else {
        return null
      }
    } else if (definition == DefinitionImpl.ENUMERATION_CONSTANT) {
      val value = id.getAttribute(SymTabKeyImpl.CONSTANT_VALUE)
      id.appendLineNumber(token.getLineNumber)

      if (sign != null) {
        PascalParserTD.errorHandler.flag(token, PascalErrorCode.INVALID_CONSTANT, this)
      }
      return value
    } else if (definition == null) {
      PascalParserTD.errorHandler.flag(token, PascalErrorCode.NOT_CONSTANT_IDENTIFIER, this)
      return null
    } else {
      PascalParserTD.errorHandler.flag(token, PascalErrorCode.INVALID_CONSTANT, this)
      return null
    }
  }

  def getConstantType(value: Any): TypeSpec = {
    var constantType: TypeSpec = null
    if (value.isInstanceOf[Int]) {
      constantType = Predefined.integerType
    } else if (value.isInstanceOf[Float]) {
      constantType = Predefined.realType
    } else if (value.isInstanceOf[String]) {
      if (value.asInstanceOf[String].length == 1) {
        constantType = Predefined.charType
      } else {
        constantType = TypeFactory.createStringType(value.asInstanceOf[String])
      }
    }
    constantType
  }

  def getConstantType(identifier: Token): TypeSpec = {
    val id = Parser.symTabStack.lookup(identifier.getText)

    if (id == null) {
      return null
    }

    val definition = id.getDefinition

    if ((definition == DefinitionImpl.CONSTANT) || (definition == DefinitionImpl.ENUMERATION_CONSTANT)) {
      return id.getTypeSpec
    } else {
      return null
    }
  }
}

/**
  * Companion object with constants.
  */
object ConstantDefinitionsParser {
  val IDENTIFIER_SET = DeclarationsParser.TYPE_START_SET.clone().asInstanceOf[util.HashSet[PascalTokenType]]
  IDENTIFIER_SET.add(PascalTokenType.IDENTIFIER)

  val CONSTANT_START_SET = new util.HashSet[PascalTokenType]
  CONSTANT_START_SET.add(PascalTokenType.IDENTIFIER)
  CONSTANT_START_SET.add(PascalTokenType.INTEGER)
  CONSTANT_START_SET.add(PascalTokenType.REAL)
  CONSTANT_START_SET.add(PascalTokenType.PLUS)
  CONSTANT_START_SET.add(PascalTokenType.MINUS)
  CONSTANT_START_SET.add(PascalTokenType.STRING)
  CONSTANT_START_SET.add(PascalTokenType.SEMICOLON)

  val EQUALS_SET = CONSTANT_START_SET.clone().asInstanceOf[util.HashSet[PascalTokenType]]
  EQUALS_SET.add(PascalTokenType.EQUALS)
  EQUALS_SET.add(PascalTokenType.SEMICOLON)

  val NEXT_START_SET = DeclarationsParser.TYPE_START_SET.clone().asInstanceOf[util.HashSet[PascalTokenType]]
  NEXT_START_SET.add(PascalTokenType.SEMICOLON)
  NEXT_START_SET.add(PascalTokenType.IDENTIFIER)
}