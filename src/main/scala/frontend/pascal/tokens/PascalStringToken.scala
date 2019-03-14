package frontend.pascal.tokens

import frontend.Source
import frontend.pascal.{PascalErrorCode, PascalToken, PascalTokenType}

/**
 * Token class for strings, which are words that start and end with "'" (single quote).
 *
 * @param source the source to read from.
 */
class PascalStringToken(source: Source) extends PascalToken(source) {
  override protected def extract(): Unit = {
    val textBuffer = new StringBuilder
    val valueBuffer = new StringBuilder

    var currentC = nextChar()
    textBuffer.append('\'')

    do {
      // Replace any whitespace character with a blank.
      if (Character.isWhitespace(currentC)) {
        currentC = ' '
      }

      if ((currentC != '\'') && (currentC != Source.EOF)) {
        textBuffer.append(currentC)
        valueBuffer.append(currentC)
        currentC = nextChar()
      }

      // Quote? Each pair of adjacent quotes represents a single-quote.
      if (currentC == '\'') {
        while ((currentC == '\'') && (peekChar() == '\'')) {
          textBuffer.append("''") // 2 quotes here
          valueBuffer.append('\'')
          nextChar()
          currentC = nextChar()
        }
      }
    } while((currentC != '\'') && (currentC != Source.EOF))

    if (currentC == '\'') {
      nextChar()
      textBuffer.append('\'')

      tokenType = PascalTokenType.STRING
      value = valueBuffer.toString()
    } else {
      tokenType = PascalTokenType.ERROR
      value = PascalErrorCode.UNEXPECTED_EOF
    }

    text = textBuffer.toString()
  }
}
