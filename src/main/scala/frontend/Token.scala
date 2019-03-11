package frontend

/**
  * The framework class that represents a token returned by the scanner.
  *
  * @param source the source to read from.
  */
class Token(protected val source: Source) {

  /**
    * Language-specific token type.
    */
  protected var tokenType: TokenType = _

  /**
    * Token text.
    */
  protected var text: String = _

  /**
    * Token value.
    * Some tokens will have values, for example 3.14159 is the an approximation of pi.
    */
  protected var value: Any = _

  /**
    * Line number of the token's source line.
    */
  protected var lineNum: Int = source.getLineNum

  /**
    * Position of the first token character.
    * This is done to record where this token begins!
    * Like the token "begin" might be in positions 11 through 15 (all inclusive).
    */
  protected var position: Int = source.getPosition

  // call extract when construction happens.
  //TODO: Check if this actually works
  extract()

  /**
    * Default method to extract only one-character tokens from the source.
    * Subclasses can override this method to construct language-specific tokens.
    * After extracting the token, the current source line position will be one beyond the last token character.
    */
  protected def extract(): Unit = {
    text = Character.toString(currentChar())
    value = null
    nextChar()
  }

  /**
    * Convenience method, alias for source.currentChar().
    *
    * @return current char from source.
    */
  protected def currentChar(): Char = {
    source.currentChar()
  }

  /**
    * Convenience method, alias for source.nextChar().
    *
    * @return next char.
    */
  protected def nextChar(): Char = {
    source.nextChar()
  }

  /**
    * Convenience method, alias for source.peekChar().
    *
    * @return peeked at char.
    */
  protected def peekChar(): Char = {
    source.peekChar()
  }
}
