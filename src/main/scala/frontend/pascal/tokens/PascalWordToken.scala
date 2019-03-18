package frontend.pascal.tokens

import frontend.Source
import frontend.pascal.{PascalToken, PascalTokenType}

/**
 * Token class for reserved words, like "begin", "program", "procedure", "record", or identifiers...
 * Refer to PascalTokenType for more.
 *
 * @param source the source to read from.
 */
class PascalWordToken(source: Source) extends PascalToken(source) {
  override protected def extract(): Unit = {
    val textBuffer = new StringBuilder
    var currentC = currentChar()

    while (Character.isLetterOrDigit(currentC)) {
      textBuffer.append(currentC)
      currentC = nextChar()
    }

    text = textBuffer.toString()

    // Is it a reserved word or an identifier?
    tokenType =
      if (PascalTokenType.RESERVED_WORDS.contains(text.toLowerCase))
        PascalTokenType.RESERVED_WORDS(text.toLowerCase)
      else
        PascalTokenType.IDENTIFIER
  }
}
