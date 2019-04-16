package frontend.pascal.parsers

import java.util

import frontend.pascal.{PascalErrorCode, PascalParserTD, PascalTokenType}
import frontend.{Parser, Token}
import intermediate.TypeSpec
import intermediate.symtabimpl.DefinitionImpl

/**
  * Simple type parser.
  *
  * @param parent parent parser.
  */
class SimpleTypeParser(parent: PascalParserTD) extends PascalParserTD(parent) {
  /**
    * Parse method.
    *
    * @param toket start token.
    * @return type spec.
    */
  def parse(toket: Token): TypeSpec = {
    var curToken = synchronize(SimpleTypeParser.SIMPLE_TYPE_START_SET)

    curToken.getTokenType.asInstanceOf[PascalTokenType] match {
      case PascalTokenType.IDENTIFIER =>
        val name = curToken.getText.toLowerCase
        val id = Parser.symTabStack.lookup(name)

        if (id != null) {
          val definition = id.getDefinition

          // It's either a type identifier
          // or the start of a subrange type.
          if (definition == DefinitionImpl.TYPE) {
            id.appendLineNumber(curToken.getLineNumber)
            curToken = nextToken() // consume the identifier

            // Return the type of the referent type.
            return id.getTypeSpec
          } else if (definition != DefinitionImpl.CONSTANT && definition != DefinitionImpl.ENUMERATION_CONSTANT) {
            PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.NOT_TYPE_IDENTIFIER, this)
            curToken = nextToken() // consume the identifier.
            return null
          } else {
            val subrangeTypeParser = new SubrangeTypeParser(this)
            return subrangeTypeParser.parse(curToken)
          }
        } else {
          PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.IDENTIFIER_UNDEFINED, this)
          curToken = nextToken() // consume the identifier
          return null
        }
      case PascalTokenType.LEFT_PAREN =>
        val enumerationTypeParser = new EnumerationTypeParser(this)
        return enumerationTypeParser.parse(curToken)
      case PascalTokenType.COMMA | PascalTokenType.SEMICOLON =>
        PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.INVALID_TYPE, this)
        return null
      case _ =>
        val subrangeTypeParser = new SubrangeTypeParser(this)
        return subrangeTypeParser.parse(curToken)
    }
  }
}

/**
  * Companion object.
  */
object SimpleTypeParser {
  val SIMPLE_TYPE_START_SET = ConstantDefinitionsParser.CONSTANT_START_SET.clone().asInstanceOf[util.HashSet[PascalTokenType]]
  SIMPLE_TYPE_START_SET.add(PascalTokenType.LEFT_PAREN)
  SIMPLE_TYPE_START_SET.add(PascalTokenType.COMMA)
  SIMPLE_TYPE_START_SET.add(PascalTokenType.SEMICOLON)
}
