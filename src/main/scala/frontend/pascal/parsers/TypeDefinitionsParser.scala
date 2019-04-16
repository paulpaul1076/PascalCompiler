package frontend.pascal.parsers

import java.util

import frontend.pascal.{PascalErrorCode, PascalParserTD, PascalTokenType}
import frontend.{Parser, Token}
import intermediate.symtabimpl.DefinitionImpl

/**
  * Type definitions parser.
  *
  * @param parent parent parser to pick up where we left off.
  */
class TypeDefinitionsParser(parent: PascalParserTD) extends PascalParserTD(parent) {

  /**
    * Parse method.
    *
    * @param toket start token.
    */
  def parse(toket: Token): Unit = {
    var curToken = synchronize(TypeDefinitionsParser.IDENTIFIER_SET)

    // Loop to parse a sequence of type definitions
    // separated by semicolons.
    while (curToken.getTokenType == PascalTokenType.IDENTIFIER) {
      val name = curToken.getText.toLowerCase
      var typeId = Parser.symTabStack.lookupLocal(name)

      // Enter the new identifier into the symbol table
      // but don't set how it's defined yet.
      if (typeId == null) {
        typeId = Parser.symTabStack.enterLocal(name)
        typeId.appendLineNumber(curToken.getLineNumber)
      } else {
        PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.IDENTIFIER_REDEFINED, this)
        typeId = null
      }

      curToken = nextToken() // consume the identifier token

      // synchronize on the = token
      curToken = synchronize(TypeDefinitionsParser.EQUALS_SET)
      if (curToken.getTokenType == PascalTokenType.EQUALS) {
        curToken = nextToken()
      } else {
        PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_EQUALS, this)
      }

      // Parse the type specification.
      val typeSpecificationParser = new TypeSpecificationParser(this)
      val typeSpec = typeSpecificationParser.parse(curToken)

      // set identifier to be a type and set its type specification.
      if (typeId != null) {
        typeId.setDefinition(DefinitionImpl.TYPE)
      }

      // Cross-link the type identifier and the type specification.
      if (typeSpec != null && typeId != null) {
        if (typeSpec.getIdentifier == null) {
          typeSpec.setIdentifier(typeId)
        }
        typeId.setTypeSpec(typeSpec)
      } else {
        curToken = synchronize(TypeDefinitionsParser.FOLLOW_SET)
      }

      curToken = currentToken()
      val tokenType = curToken.getTokenType

      // Look for one or more semicolons after a definition.
      if (tokenType == PascalTokenType.SEMICOLON) {
        while (curToken.getTokenType == PascalTokenType.SEMICOLON) {
          curToken = nextToken() // consume the ;
        }
      }

      // If at the start of the next definition or declaration,
      // then missing a semicolon
      else if (TypeDefinitionsParser.NEXT_START_SET.contains(tokenType)) {
        PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_SEMICOLON, this)
      }

      curToken = synchronize(TypeDefinitionsParser.IDENTIFIER_SET)
    }
  }
}

/**
  * Companion object with constants.
  */
object TypeDefinitionsParser {
  val IDENTIFIER_SET = DeclarationsParser.VAR_START_SET.clone().asInstanceOf[util.HashSet[PascalTokenType]]
  IDENTIFIER_SET.add(PascalTokenType.IDENTIFIER)

  val EQUALS_SET = ConstantDefinitionsParser.CONSTANT_START_SET.clone().asInstanceOf[util.HashSet[PascalTokenType]]
  EQUALS_SET.add(PascalTokenType.EQUALS)
  EQUALS_SET.add(PascalTokenType.SEMICOLON)

  val FOLLOW_SET = new util.HashSet[PascalTokenType]()
  FOLLOW_SET.add(PascalTokenType.SEMICOLON)

  val NEXT_START_SET = DeclarationsParser.VAR_START_SET.clone().asInstanceOf[util.HashSet[PascalTokenType]]
  NEXT_START_SET.add(PascalTokenType.SEMICOLON)
  NEXT_START_SET.add(PascalTokenType.IDENTIFIER)
}