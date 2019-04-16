package frontend.pascal.parsers

import java.util

import frontend.Token
import frontend.pascal.{PascalParserTD, PascalTokenType}
import intermediate.TypeSpec

/**
  * Type specification parser class.
  *
  * @param parent parent parser to pick up where we left off.
  */
class TypeSpecificationParser(parent: PascalParserTD) extends PascalParserTD(parent) {

  /**
    * Parse method.
    *
    * @param toket start token.
    * @return type specficication object, which is like a symbol table for a type.
    */
  def parse(toket: Token): TypeSpec = {
    var curToken = synchronize(TypeSpecificationParser.TYPE_START_SET)

    curToken.getTokenType.asInstanceOf[PascalTokenType] match {
      case PascalTokenType.ARRAY =>
        val arrayTypeParser = new ArrayTypeParser(this)
        arrayTypeParser.parse(curToken)
      case PascalTokenType.RECORD =>
        val recordTypeParser = new RecordTypeParser(this)
        recordTypeParser.parse(curToken)
      case _ =>
        val simpleTypeParser = new SimpleTypeParser(this)
        simpleTypeParser.parse(curToken)
    }
  }
}

/**
  * Companion object with constants.
  */
object TypeSpecificationParser {
  // synchronization set for starting a type specification.
  val TYPE_START_SET = SimpleTypeParser.SIMPLE_TYPE_START_SET.clone().asInstanceOf[util.HashSet[PascalTokenType]]
  TYPE_START_SET.add(PascalTokenType.ARRAY)
  TYPE_START_SET.add(PascalTokenType.RECORD)
  TYPE_START_SET.add(PascalTokenType.SEMICOLON)
}
