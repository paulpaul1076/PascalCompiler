package frontend.pascal.tokens

import frontend.Source
import frontend.pascal.{PascalErrorCode, PascalToken, PascalTokenType}

/**
 * Token class for numbers, which can be reals or ints.
 *
 * @param source the source to read from.
 */
class PascalNumberToken(source: Source) extends PascalToken(source) {
  override protected def extract(): Unit = {
    val textBuffer = new StringBuilder
    extractNumber(textBuffer)
    text = textBuffer.toString()
  }

  /**
   * Extract a number token.
   *
   * @param textBuffer buffer with the number string.
   */
  protected def extractNumber(textBuffer: StringBuilder): Unit = {
    var wholeDigits: String = null
    var fractionDigits: String = null
    var exponentDigits: String = null
    var exponentSign = '+'
    var sawDotDot = false

    tokenType = PascalTokenType.INTEGER // just an assumption

    // Extract digits of the whole part of a number
    wholeDigits = unsignedIntegerDigits(textBuffer)
    if (tokenType == PascalTokenType.ERROR) {
      return
    }

    // Is there a .?
    // It could be a decimal point or the start of a .. token.
    var currentC = currentChar()
    if (currentC == '.') {
      if (peekChar() == '.') {
        sawDotDot = true // it's a ".." token, so don't consume it
      } else {
        tokenType = PascalTokenType.REAL
        textBuffer.append(currentC)
        currentC = nextChar()

        // Collect the digits of the fraction part of the number.
        fractionDigits = unsignedIntegerDigits(textBuffer)
        if (tokenType == PascalTokenType.ERROR) {
          return
        }
      }
    }

    // Is there an exponent part?
    // There cannot be an exponent if we already saw a ".." token.
    currentC = currentChar()
    if (!sawDotDot && ((currentC == 'E') || (currentC == 'e'))) {
      tokenType = PascalTokenType.REAL
      textBuffer.append(currentC)
      currentC = nextChar() // consume 'E' or 'e'

      //Exponent sign?
      if ((currentC == '+') || (currentC == '-')) {
        textBuffer.append(currentC)
        exponentSign = currentC
        currentC = nextChar() // consume '+' or '-'
      }
      //Extract the digits of the exponent
      exponentDigits = unsignedIntegerDigits(textBuffer)
    }

    //Compute the value of an integer number token.
    if (tokenType == PascalTokenType.INTEGER) {
      val integerValue: Int = computeIntegerValue(wholeDigits)
      if (tokenType != PascalTokenType.ERROR) {
        value = integerValue
      }
    }
    // Compute the value of a real number token.
    else if (tokenType == PascalTokenType.REAL) {
      val floatValue: Float = computeFloatValue(wholeDigits, fractionDigits, exponentDigits, exponentSign)
      if (tokenType != PascalTokenType.ERROR) {
        value = floatValue
      }
    }
  }

  private def unsignedIntegerDigits(textBuffer: StringBuilder): String = {
    var currentC = currentChar()

    // Must have at least one digit
    // TODO: Well otherwise we would not have come here, right? Check this.
    if (!Character.isDigit(currentC)) {
      tokenType = PascalTokenType.ERROR
      value = PascalErrorCode.INVALID_NUMBER
      return null
    }

    //Extract the digits
    val digits = new StringBuilder
    while (Character.isDigit(currentC)) {
      digits.append(currentC)
      textBuffer.append(currentC)
      currentC = nextChar()
    }
    digits.toString()
  }

  /**
   * Compute and return the integer value of a string of digits.
   * Check for overflow.
   *
   * @param digits the string of digits.
   * @return the integer value.
   */
  private def computeIntegerValue(digits: String): Int = {
    // Return 0 if no digits
    if (digits == null) {
      return 0
    }

    var integerValue = 0
    var prevValue = -1 // overflow occurred if prevValue > integerValue
    var index = 0

    // Loop over the digits to compute the integer value
    // as long as there is no overflow.
    while ((index < digits.length) && (integerValue >= prevValue)) {
      prevValue = integerValue
      integerValue = 10 * integerValue + Character.getNumericValue(digits(index))
      index += 1
    }

    // No overflow: return the integer value.
    if (integerValue >= prevValue) {
      return integerValue
    }
    // Overflow: set the integer out of range error
    else {
      tokenType = PascalTokenType.ERROR
      value = PascalErrorCode.RANGE_INTEGER
      return 0
    }
  }

  /**
   * Compute and return the float value of a real number.
   *
   * @param wholeDigits    the digits before the dot.
   * @param fractionDigits the digits after the dot.
   * @param exponentDigits exponent digits.
   * @param exponentSign   the sign of the exponent.
   * @return the float value of our number.
   */
  private def computeFloatValue(wholeDigits: String, fractionDigits: String, exponentDigits: String, exponentSign: Char): Float = {
    var floatValue = 0.0
    var exponentValue = computeIntegerValue(exponentDigits)
    var digits = wholeDigits

    // negate the exponent sign if the exponent sign is '-'.
    if (exponentSign == '-') {
      exponentValue = -exponentValue
    }

    // if There are any fraction digits, adjust the exponent value
    // and append the fraction digits.
    if (fractionDigits != null) {
      exponentValue -= fractionDigits.length
      digits += fractionDigits
    }

    // Check for a real number out of range error.
    if (Math.abs(exponentValue + wholeDigits.length) > PascalNumberToken.MAX_EXPONENT) {
      tokenType = PascalTokenType.ERROR
      value = PascalErrorCode.RANGE_REAL
      return 0.0f // why the 'f' in there?
    }

    // Loop over the digits to compute the float value.
    var index = 0
    while (index < digits.length) {
      floatValue = 10 * floatValue + Character.getNumericValue(digits(index))
      index += 1
    }

    //Adjust the float value based on the exponent value.
    if (exponentValue != 0) {
      floatValue *= Math.pow(10, exponentValue)
    }
    return floatValue.asInstanceOf[Float]
  }
}

/**
 * Companion object.
 */
private object PascalNumberToken {
  val MAX_EXPONENT = 37
}