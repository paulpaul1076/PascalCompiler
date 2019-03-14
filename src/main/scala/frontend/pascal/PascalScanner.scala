package frontend.pascal

import frontend.pascal.tokens.{PascalNumberToken, PascalSpecialSymbolToken, PascalStringToken, PascalWordToken}
import frontend.{EofToken, Scanner, Source, Token}

/**
 * Pascal scanner.
 *
 * @param source source file.
 */
class PascalScanner(source: Source) extends Scanner(source) {
  /**
   * Do the actual work of extracting and returning the next token from the source.
   * Implemented by language specific scanner classes.
   *
   * Token's constructors increment source's position by 1.
   *
   * @return next token in the input source.
   */
  override protected def extractToken(): Token = {
    skipWhiteSpace()

    val currentC = currentChar()
    var token: Token = null
    if (currentC == Source.EOF) {
      token = new EofToken(source, PascalTokenType.END_OF_FILE) // TODO: fix this.
    } else if (Character.isLetter(currentC)) {
      token = new PascalWordToken(source)
    } else if (Character.isDigit(currentC)) {
      token = new PascalNumberToken(source)
    } else if (currentC == '\'') {
      token = new PascalStringToken(source)
    } else if (PascalTokenType.SPECIAL_SYMBOLS.contains(Character.toString(currentC))) {
      token = new PascalSpecialSymbolToken(source)
    } else {
      token = new PascalErrorToken(source, PascalErrorCode.INVALID_CHARACTER, Character.toString(currentC))
      nextChar()
    }

    token
  }

  /**
   * Skips whitespace and comments.
   */
  private def skipWhiteSpace(): Unit = {
    var currentC = currentChar()
    while (Character.isWhitespace(currentC) || currentC == '{') {
      if (currentC == '{') {
        do {
          currentC = nextChar()
        } while (currentC != '}' && currentC != Source.EOF) // TODO: Is this valid EOF at all? find out!

        // if the last consumed char is not EOF and is '}', then consume it.
        if (currentC == '}') {
          currentC = nextChar()
        }
      } else {
        currentC = nextChar()
      }
    }
  }
}
