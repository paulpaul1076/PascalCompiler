package frontend.pascal

import frontend.Source

/**
 * Pascal error token.
 *
 * @param source    the source from where to fetch subsequent characters.
 * @param errorCode the error code.
 * @param tokenText the text of the erroneous token.
 */
class PascalErrorToken(source: Source, val errorCode: PascalErrorCode, val tokenText: String)
  extends PascalToken(source) {

  text = tokenText
  tokenType = PascalTokenType.ERROR
  value = errorCode

  /**
   * Do nothing. Do not consume any source characters.
   */
  override protected def extract(): Unit = {}
}
