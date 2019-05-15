package frontend.pascal.parsers

import java.util

import frontend.pascal.{PascalErrorCode, PascalParserTD, PascalTokenType}
import frontend.{Parser, Token}
import intermediate.symtabimpl.DefinitionImpl
import intermediate.{Definition, SymTabEntry, TypeSpec}

import scala.collection.JavaConverters._

/**
  * Parser of variable declarations.
  *
  * @param parent parent parser.
  */
class VariableDeclarationsParser(parent: PascalParserTD) extends PascalParserTD(parent) {

  var definition: Definition = _

  def setDefinition(definition: Definition): Unit = {
    this.definition = definition
  }

  /**
    * Parse method.
    *
    * @param toket token.
    */
  def parse(toket: Token): Unit = {
    var curToken = synchronize(VariableDeclarationsParser.IDENTIFIER_SET)

    // separated by semicolons.
    while (curToken.getTokenType == PascalTokenType.IDENTIFIER) {
      // Parse the identifier sublist and its type specification.
      parseIdentifierSublist(curToken, VariableDeclarationsParser.IDENTIFIER_FOLLOW_SET, VariableDeclarationsParser.COMMA_SET)

      curToken = currentToken()
      val tokenType = curToken.getTokenType

      // Look for one or more semicolons after a definition.
      if (tokenType == PascalTokenType.SEMICOLON) {
        while (curToken.getTokenType == PascalTokenType.SEMICOLON) {
          curToken = nextToken() // consume the ;
        }
      }
      // If at the start of the next definition or declaration,
      // then missing a semicolon.
      else if (VariableDeclarationsParser.NEXT_START_SET.contains(tokenType)) {
        PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_SEMICOLON, this)
      }
      curToken = synchronize(VariableDeclarationsParser.IDENTIFIER_SET)
    }
  }

  private def parseIdentifier(token: Token): SymTabEntry = {
    var id: SymTabEntry = null

    if (token.getTokenType == PascalTokenType.IDENTIFIER) {
      val name = token.getText.toLowerCase()
      id = Parser.symTabStack.lookupLocal(name)

      // Enter a new identifier into the symbol table.
      if (id == null) {
        id = Parser.symTabStack.enterLocal(name)
        id.setDefinition(definition)
        id.appendLineNumber(token.getLineNumber)
      } else {
        PascalParserTD.errorHandler.flag(token, PascalErrorCode.IDENTIFIER_REDEFINED, this)
      }
      nextToken() // consume the identifier token
    } else {
      PascalParserTD.errorHandler.flag(token, PascalErrorCode.MISSING_IDENTIFIER, this)
    }

    id
  }

  def parseIdentifierSublist(toket: Token,
                             followSet: util.HashSet[PascalTokenType],
                             commaSet: util.HashSet[PascalTokenType]): util.ArrayList[SymTabEntry] = {
    var curToken = toket
    val sublist = new util.ArrayList[SymTabEntry]()

    do {
      curToken = synchronize(VariableDeclarationsParser.IDENTIFIER_START_SET)
      val id = parseIdentifier(curToken)

      if (id != null) {
        sublist.add(id)
      }

      curToken = synchronize(commaSet)
      val tokenType = curToken.getTokenType

      // Look for the comma.
      if (tokenType == PascalTokenType.COMMA) {
        curToken = nextToken() // consume the comma

        if (followSet.contains(curToken.getTokenType)) {
          PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_IDENTIFIER, this)
        }
      } else if (VariableDeclarationsParser.IDENTIFIER_START_SET.contains(tokenType)) {
        PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_COMMA, this)
      }
    } while (!followSet.contains(curToken.getTokenType))

    if (definition != DefinitionImpl.PROGRAM_PARM) {
      // Parse the type specification.
      val `type` = parseTypeSpec(curToken)

      // Assign the type specification to each identifier in the list.
      for (variableId: SymTabEntry <- sublist.asScala) {
        variableId.setTypeSpec(`type`)
      }
    }

    sublist
  }

  def parseTypeSpec(token: Token): TypeSpec = {
    // Synchronize on the : token.
    var curToken = synchronize(VariableDeclarationsParser.COLON_SET)
    if (curToken.getTokenType == PascalTokenType.COLON) {
      curToken = nextToken() // consume the :
    } else {
      PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_COLON, this)
    }

    // Parse the type specification
    val typeSpecificationParser = new TypeSpecificationParser(this)
    val typeSpec = typeSpecificationParser.parse(curToken)

    // Formal parameters and functions must have named types.
    if (definition != DefinitionImpl.VARIABLE &&
      definition != DefinitionImpl.FIELD &&
      typeSpec != null &&
      typeSpec.getIdentifier == null) {

      PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.INVALID_TYPE, this)
    }

    typeSpec
  }
}

/**
  * Companion object for constants, etc.
  */
object VariableDeclarationsParser {
  val IDENTIFIER_SET = DeclarationsParser.VAR_START_SET.clone().asInstanceOf[util.HashSet[PascalTokenType]]
  IDENTIFIER_SET.add(PascalTokenType.IDENTIFIER)
  IDENTIFIER_SET.add(PascalTokenType.END)
  IDENTIFIER_SET.add(PascalTokenType.SEMICOLON)

  val NEXT_START_SET = DeclarationsParser.ROUTINE_START_SET.clone().asInstanceOf[util.HashSet[PascalTokenType]]
  NEXT_START_SET.add(PascalTokenType.IDENTIFIER)
  NEXT_START_SET.add(PascalTokenType.SEMICOLON)

  val IDENTIFIER_START_SET = new util.HashSet[PascalTokenType]()
  IDENTIFIER_START_SET.add(PascalTokenType.IDENTIFIER)
  IDENTIFIER_START_SET.add(PascalTokenType.COMMA)

  val IDENTIFIER_FOLLOW_SET = new util.HashSet[PascalTokenType]()
  IDENTIFIER_FOLLOW_SET.add(PascalTokenType.COLON)
  IDENTIFIER_FOLLOW_SET.add(PascalTokenType.SEMICOLON)
  IDENTIFIER_FOLLOW_SET.addAll(DeclarationsParser.VAR_START_SET)

  val COMMA_SET = new util.HashSet[PascalTokenType]()
  COMMA_SET.add(PascalTokenType.COMMA)
  COMMA_SET.add(PascalTokenType.COLON)
  COMMA_SET.add(PascalTokenType.IDENTIFIER)
  COMMA_SET.add(PascalTokenType.SEMICOLON)

  val COLON_SET = new util.HashSet[PascalTokenType]
  COLON_SET.add(PascalTokenType.COLON)
  COLON_SET.add(PascalTokenType.SEMICOLON)
}
