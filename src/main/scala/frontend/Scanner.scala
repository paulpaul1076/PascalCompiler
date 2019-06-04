package frontend

/**
  * Scanner class to be extended by language specific scanner classes.
  * @param source source file.
  */
abstract class Scanner(protected val source: Source) {
  /**
    * Current token storage.
    */
  private var currentTokenVar: Token = _

  /**
    * Return the current token from the source.
    * @return current token.
    */
  def currentToken(): Token = {
    currentTokenVar
  }

  /**
    * Return next token from the source.
    * @return the next token.
    */
  def nextToken(): Token = {
    currentTokenVar = extractToken()
    currentTokenVar
  }

  /**
    * Do the actual work of extracting and returning the next token from the source.
    * Implemented by language specific scanner classes.
    * @return next token in the input source.
    */
  protected def extractToken(): Token

  /**
    * Convenience method, alias for source.currentChar().
    *
    * @return current char in source.
    */
  def currentChar(): Char = {
    source.currentChar()
  }

  /**
    * Convenience method, alias for source.nextChar().
    *
    * @return next char in source.
    */
  def nextChar(): Char = {
    source.nextChar()
  }

  /**
    * Call the source's atEol() method.
    * @return true if at the end of the source line, else return false.
    */
  def atEol(): Boolean = source.atEol()

  /**
    * Call the source's atEof() method.
    * @return true if at the end of the source file, else return false.
    */
  def atEof(): Boolean = source.atEof()

  /**
    * Call the source's skipToNextLine() method.
    */
  def skipToNextLine(): Unit = source.skipToNextLine()
}
