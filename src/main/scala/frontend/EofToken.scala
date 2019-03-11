package frontend

/**
  * Eof of file token.
  * @param source the source to read from.
  */
class EofToken(source: Source) extends Token(source) {

  /**
    * Do nothing. Do not consume any source characters.
    *
    * @param source the source to read from.
    */
  protected def extract(source: Source): Unit = {

  }
}
