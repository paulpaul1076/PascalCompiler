package frontend.pascal.parsers

import java.util

import frontend.Token
import frontend.pascal.{PascalErrorCode, PascalParserTD, PascalTokenType}
import intermediate.typeimpl.{TypeFormImpl, TypeKeyImpl}
import intermediate.{SymTabEntry, TypeFactory, TypeSpec}

/**
  * Array parser class.
  *
  * @param parent parent parser class to pick up where we left off.
  */
class ArrayTypeParser(parent: PascalParserTD) extends PascalParserTD(parent) {

  /**
    * Parse method.
    *
    * @param toket start token.
    * @return type spec object.
    */
  def parse(toket: Token): TypeSpec = {
    val arrayType = TypeFactory.createType(TypeFormImpl.ARRAY)
    var curToken = nextToken() // consume ARRAY

    // Synchronize at the [ token
    curToken = synchronize(ArrayTypeParser.LEFT_BRACKET_SET)
    if (curToken.getTokenType != PascalTokenType.LEFT_BRACKET) {
      PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_LEFT_BRACKET, this)
    }
    //Parse the list of index types.
    val elementType = parseIndexTypeList(curToken, arrayType)

    // synchronize at the ] token
    curToken = synchronize(ArrayTypeParser.RIGHT_BRACKET_SET)
    if (curToken.getTokenType == PascalTokenType.RIGHT_BRACKET) {
      curToken = nextToken()
    } else {
      PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_RIGHT_BRACKET, this)
    }

    // Synchronize at OF.
    curToken = synchronize(ArrayTypeParser.OF_SET)
    if (curToken.getTokenType == PascalTokenType.OF) {
      curToken = nextToken() // consume OF
    } else {
      PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_OF, this)
    }

    // Parse the element type.
    elementType.setAttribute(TypeKeyImpl.ARRAY_ELEMENT_TYPE, parseElementType(curToken))

    arrayType
  }

  private def parseIndexTypeList(token: Token, arrayType: TypeSpec): TypeSpec = {
    var elementType = arrayType
    var anotherIndex = false

    var curToken = nextToken() // consume the [ token

    // Parse the list of index type specifications.
    do {
      anotherIndex = false

      // Parse the index type.
      curToken = synchronize(ArrayTypeParser.INDEX_START_SET)
      parseIndexType(curToken, elementType)

      // Synchronize at the , token.
      curToken = synchronize(ArrayTypeParser.INDEX_FOLLOW_SET)
      val tokenType = curToken.getTokenType

      if (tokenType != PascalTokenType.COMMA && tokenType != PascalTokenType.RIGHT_BRACKET) {
        if (ArrayTypeParser.INDEX_START_SET.contains(tokenType)) {
          PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_COMMA, this)
          anotherIndex = true
        }
      }

      // Create an ARRAY element type object
      // for each subsequent index type.
      else if (tokenType == PascalTokenType.COMMA) {
        val newElementType = TypeFactory.createType(TypeFormImpl.ARRAY)
        elementType.setAttribute(TypeKeyImpl.ARRAY_ELEMENT_TYPE, newElementType)
        elementType = newElementType

        curToken = nextToken() // consume the , token
        anotherIndex = true
      }
    } while (anotherIndex)

    elementType
  }

  private def parseIndexType(token: Token, arrayType: TypeSpec): Unit = {
    val simpleTypeParser = new SimpleTypeParser(this)
    val indexType = simpleTypeParser.parse(token)
    arrayType.setAttribute(TypeKeyImpl.ARRAY_INDEX_TYPE, indexType)

    if (indexType == null) {
      return
    }

    val form = indexType.getForm
    var count = 0

    // Check the index type and set the element count.
    if (form == TypeFormImpl.SUBRANGE) {
      val minValue = indexType.getAttribute(TypeKeyImpl.SUBRANGE_MIN_VALUE).asInstanceOf[Integer]
      val maxValue = indexType.getAttribute(TypeKeyImpl.SUBRANGE_MAX_VALUE).asInstanceOf[Integer]

      if (minValue != null && maxValue != null) {
        count = maxValue - minValue + 1
      }
    } else if (form == TypeFormImpl.ENUMERATION) {
      val constants = indexType.getAttribute(TypeKeyImpl.ENUMERATION_CONSTANTS).asInstanceOf[util.ArrayList[SymTabEntry]]
      count = constants.size()
    } else {
      PascalParserTD.errorHandler.flag(token, PascalErrorCode.INVALID_INDEX_TYPE, this)
    }
    arrayType.setAttribute(TypeKeyImpl.ARRAY_ELEMENT_COUNT, count)
  }

  private def parseElementType(token: Token): TypeSpec = {
    val typeSpecificationParser = new TypeSpecificationParser(this)

    typeSpecificationParser.parse(token)
  }
}

/**
  * Companion object with constants.
  */
object ArrayTypeParser {
  val LEFT_BRACKET_SET = SimpleTypeParser.SIMPLE_TYPE_START_SET.clone().asInstanceOf[util.HashSet[PascalTokenType]]
  LEFT_BRACKET_SET.add(PascalTokenType.LEFT_BRACKET)
  LEFT_BRACKET_SET.add(PascalTokenType.RIGHT_BRACKET)

  val RIGHT_BRACKET_SET = new util.HashSet[PascalTokenType]()
  RIGHT_BRACKET_SET.add(PascalTokenType.RIGHT_BRACKET)
  RIGHT_BRACKET_SET.add(PascalTokenType.OF)
  RIGHT_BRACKET_SET.add(PascalTokenType.SEMICOLON)

  val OF_SET = TypeSpecificationParser.TYPE_START_SET.clone().asInstanceOf[util.HashSet[PascalTokenType]]
  OF_SET.add(PascalTokenType.OF)
  OF_SET.add(PascalTokenType.SEMICOLON)

  val INDEX_START_SET = SimpleTypeParser.SIMPLE_TYPE_START_SET.clone().asInstanceOf[util.HashSet[PascalTokenType]]
  INDEX_START_SET.add(PascalTokenType.COMMA)

  val INDEX_END_SET = new util.HashSet[PascalTokenType]()
  INDEX_END_SET.add(PascalTokenType.RIGHT_BRACKET)
  INDEX_END_SET.add(PascalTokenType.OF)
  INDEX_END_SET.add(PascalTokenType.SEMICOLON)

  val INDEX_FOLLOW_SET = INDEX_START_SET.clone().asInstanceOf[util.HashSet[PascalTokenType]]
  INDEX_FOLLOW_SET.addAll(INDEX_END_SET)
}