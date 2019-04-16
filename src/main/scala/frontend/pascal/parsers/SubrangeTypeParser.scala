package frontend.pascal.parsers

import frontend.Token
import frontend.pascal.{PascalErrorCode, PascalParserTD, PascalTokenType}
import intermediate.symtabimpl.Predefined
import intermediate.typeimpl.{TypeFormImpl, TypeKeyImpl}
import intermediate.{TypeFactory, TypeSpec}

class SubrangeTypeParser(parent: PascalParserTD) extends PascalParserTD(parent) {
  def parse(token: Token): TypeSpec = {
    val subrangeType = TypeFactory.createType(TypeFormImpl.SUBRANGE)
    var minValue: Any = null
    var maxValue: Any = null

    // Parse the minimum constant.
    var constantToken = token
    val constantParser = new ConstantDefinitionsParser(this)
    minValue = constantParser.parseConstant(token)

    // Set the minimum constant's type.
    val minType =
      if (constantToken.getTokenType == PascalTokenType.IDENTIFIER)
        constantParser.getConstantType(constantToken)
      else
        constantParser.getConstantType(minValue)

    minValue = checkValueType(constantToken, minValue, minType)

    var curToken = currentToken()

    var sawDotDot = false

    // Look for the .. token.
    if (curToken.getTokenType == PascalTokenType.DOT_DOT) {
      curToken = nextToken() // consume the .. token
      sawDotDot = true
    }

    val tokenType = curToken.getTokenType

    // At the start of the maximum constant?
    if (ConstantDefinitionsParser.CONSTANT_START_SET.contains(tokenType)) {
      if (!sawDotDot) {
        PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_DOT_DOT, this)
      }

      // Parse the maximum constant.
      curToken = synchronize(ConstantDefinitionsParser.CONSTANT_START_SET)
      constantToken = curToken
      maxValue = constantParser.parseConstant(curToken)

      //Set the maximum constant's type.
      val maxType =
        if (constantToken.getTokenType == PascalTokenType.IDENTIFIER)
          constantParser.getConstantType(constantToken)
        else
          constantParser.getConstantType(maxValue)

      maxValue = checkValueType(constantToken, maxValue, maxType)

      // Are the min and max value types valid?
      if ((minType == null) || (maxType == null)) {
        PascalParserTD.errorHandler.flag(constantToken, PascalErrorCode.INCOMPATIBLE_TYPES, this)
      }

      // Are the min and max value types the same?
      else if (minType != maxType) {
        PascalParserTD.errorHandler.flag(constantToken, PascalErrorCode.INVALID_SUBRANGE_TYPE, this)
      }

      // Min value > max value?
      else if ((minValue != null) && (maxValue != null) && (minValue.asInstanceOf[Int] >= maxValue.asInstanceOf[Int])) {
        PascalParserTD.errorHandler.flag(constantToken, PascalErrorCode.MIN_GT_MAX, this)
      }
    } else {
      PascalParserTD.errorHandler.flag(constantToken, PascalErrorCode.INVALID_SUBRANGE_TYPE, this)
    }

    subrangeType.setAttribute(TypeKeyImpl.SUBRANGE_BASE_TYPE, minType)
    subrangeType.setAttribute(TypeKeyImpl.SUBRANGE_MIN_VALUE, minValue)
    subrangeType.setAttribute(TypeKeyImpl.SUBRANGE_MAX_VALUE, maxValue)

    subrangeType
  }

  /**
    * Check a value of a type specification.
    *
    * @param token    the current token.
    * @param value    the value.
    * @param typeSpec the type specification.
    * @return the value.
    */
  private def checkValueType(token: Token, value: Any, typeSpec: TypeSpec): Any = {
    if (typeSpec == null) {
      return value
    }
    if (typeSpec == Predefined.integerType) {
      return value
    } else if (typeSpec == Predefined.charType) {
      val ch = value.asInstanceOf[String].charAt(0)
      return Character.getNumericValue(ch)
    } else if (typeSpec.getForm == TypeFormImpl.ENUMERATION) {
      return value
    } else {
      PascalParserTD.errorHandler.flag(token, PascalErrorCode.INVALID_SUBRANGE_TYPE, this)
      return value
    }
  }
}
