package frontend.pascal.parsers

import java.util

import frontend.pascal.{PascalErrorCode, PascalParserTD, PascalTokenType}
import frontend.{Parser, Token}
import intermediate.symtabimpl.{DefinitionImpl, SymTabKeyImpl}
import intermediate.typeimpl.{TypeFormImpl, TypeKeyImpl}
import intermediate.{SymTabEntry, TypeFactory, TypeSpec}

class EnumerationTypeParser(parent: PascalParserTD) extends PascalParserTD(parent) {
  def parse(token: Token): TypeSpec = {
    val enumerationType = TypeFactory.createType(TypeFormImpl.ENUMERATION)
    var value = -1

    val constants = new util.ArrayList[SymTabEntry]
    var curToken = nextToken() // consume the opening (

    do {
      curToken = synchronize(EnumerationTypeParser.ENUM_CONSTANT_START_SET)
      value += 1
      parseEnumerationIdentifier(curToken, value, enumerationType, constants)

      curToken = currentToken()
      var tokenType = curToken.getTokenType

      // Look for the comma.
      if (tokenType == PascalTokenType.COMMA) {
        curToken = nextToken() // consume the comma

        if (EnumerationTypeParser.ENUM_DEFINITION_FOLLOW_SET.contains(curToken.getTokenType)) {
          PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_IDENTIFIER, this)
        }
      } else if (EnumerationTypeParser.ENUM_CONSTANT_START_SET.contains(tokenType)) {
        PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_COMMA, this)
      }
    } while (!EnumerationTypeParser.ENUM_DEFINITION_FOLLOW_SET.contains(curToken.getTokenType))

    // Look for the closing ).
    if (curToken.getTokenType == PascalTokenType.RIGHT_PAREN) {
      curToken = nextToken() // consume the )
    } else {
      PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_RIGHT_PAREN, this)
    }

    enumerationType.setAttribute(TypeKeyImpl.ENUMERATION_CONSTANTS, constants)

    enumerationType
  }

  def parseEnumerationIdentifier(token: Token, value: Int, enumerationType: TypeSpec, constants: util.ArrayList[SymTabEntry]): Unit = {
    val tokenType = token.getTokenType
    if (tokenType == PascalTokenType.IDENTIFIER) {
      val name = token.getText.toLowerCase
      var constantId = Parser.symTabStack.lookupLocal(name)

      if (constantId != null) {
        PascalParserTD.errorHandler.flag(token, PascalErrorCode.IDENTIFIER_REDEFINED, this)
      } else {
        constantId = Parser.symTabStack.enterLocal(token.getText)
        constantId.setDefinition(DefinitionImpl.ENUMERATION_CONSTANT)
        constantId.setTypeSpec(enumerationType)
        constantId.setAttribute(SymTabKeyImpl.CONSTANT_VALUE, value)
        constantId.appendLineNumber(token.getLineNumber)
        constants.add(constantId)
      }
      nextToken() // consume the identifier
    } else {
      PascalParserTD.errorHandler.flag(token, PascalErrorCode.MISSING_IDENTIFIER, this)
    }
  }
}

object EnumerationTypeParser {
  val ENUM_CONSTANT_START_SET = new util.HashSet[PascalTokenType]()
  ENUM_CONSTANT_START_SET.add(PascalTokenType.IDENTIFIER)
  ENUM_CONSTANT_START_SET.add(PascalTokenType.SEMICOLON)

  val ENUM_DEFINITION_FOLLOW_SET = new util.HashSet[PascalTokenType]()
  ENUM_DEFINITION_FOLLOW_SET.add(PascalTokenType.RIGHT_PAREN)
  ENUM_DEFINITION_FOLLOW_SET.add(PascalTokenType.SEMICOLON)
  ENUM_DEFINITION_FOLLOW_SET.addAll(DeclarationsParser.VAR_START_SET)
}
