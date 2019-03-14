package frontend.pascal.tokens

import frontend.Source
import frontend.pascal.{PascalErrorCode, PascalToken, PascalTokenType}

/**
 * Token class for special symbols, like "+","-",">",">=","[","..", etc... Refer to PascalTokenType for more.
 *
 * @param source the source to read from.
 */
class PascalSpecialSymbolToken(source: Source) extends PascalToken(source) {
  override protected def extract(): Unit = {
    var currentC = currentChar()

    text = Character.toString(currentC)
    tokenType = null

    currentC match {
      // TODO: Answer: How is '\'' even use? Does it not case ambiguity between PascalSpecialSymbolToken and PascalStringToken????????
      case '+' | '-' | '*' | '/' | ',' | ';' | '\'' | '=' | '(' | ')' | '[' | ']' | '{' | '}' | '^' =>
        nextChar()
      case ':' =>
        currentC = nextChar()
        if (currentC == '=') {
          text += currentC
          nextChar()
        }
      case '<' =>
        currentC = nextChar()
        if(currentC == '=') {
          text += currentC
          nextChar()
        } else if (currentC == '>') {
          text += currentC
          nextChar()
        }
      case '>' =>
        currentC = nextChar()
        if(currentC == '=') {
          text += currentC
          nextChar()
        }
      case '.' =>
        currentC = nextChar()
        if(currentC == '.') {
          text += currentC
          nextChar()
        }
      case _ =>
        nextChar()
        tokenType = PascalTokenType.ERROR
        value = PascalErrorCode.INVALID_CHARACTER
    }

    if(tokenType == null) {
      tokenType = PascalTokenType.SPECIAL_SYMBOLS(text)
    }
  }
}
