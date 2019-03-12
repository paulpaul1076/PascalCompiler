package frontend.pascal

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
   * @return next token in the input source.
   */
  override protected def extractToken(): Token = {
    val currentChar = currentChar()

    currentChar match {
      case Source.EOF => new EofToken(source)
      case _          => new Token(source)
    }
    // TODO: find out how the method moves the current reading position. p. 33
  }
}
