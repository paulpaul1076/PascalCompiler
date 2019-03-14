package frontend.pascal

import frontend.TokenType

import scala.collection.mutable

/**
 * Pascal token type.
 *
 * @param text text.
 */
class PascalTokenType(private var text: String) extends TokenType {
  def this() {
    this(this.toString.toLowerCase)
  }

  def getText: String = text
}

/**
 * Pascal token types.
 */
object PascalTokenType {
  // Reserved words.
  val AND = new PascalTokenType()
  val ARRAY = new PascalTokenType()
  val BEGIN = new PascalTokenType()
  val CASE = new PascalTokenType()
  val CONST = new PascalTokenType()
  val DIV = new PascalTokenType()
  val DO = new PascalTokenType()
  val DOWNTO = new PascalTokenType()
  val ELSE = new PascalTokenType()
  val END = new PascalTokenType()
  val FILE = new PascalTokenType()
  val FOR = new PascalTokenType()
  val FUNCTION = new PascalTokenType()
  val GOTO = new PascalTokenType()
  val IF = new PascalTokenType()
  val IN = new PascalTokenType()
  val LABEL = new PascalTokenType()
  val MOD = new PascalTokenType()
  val NIL = new PascalTokenType()
  val NOT = new PascalTokenType()
  val OF = new PascalTokenType()
  val OR = new PascalTokenType()
  val PACKED = new PascalTokenType()
  val PROCEDURE = new PascalTokenType()
  val PROGRAM = new PascalTokenType()
  val RECORD = new PascalTokenType()
  val REPEAT = new PascalTokenType()
  val SET = new PascalTokenType()
  val THEN = new PascalTokenType()
  val TO = new PascalTokenType()
  val TYPE = new PascalTokenType()
  val UNTIL = new PascalTokenType()
  val VAR = new PascalTokenType()
  val WHILE = new PascalTokenType()
  val WITH = new PascalTokenType()

  // Special symbols.
  val PLUS = new PascalTokenType("+")
  val MINUS = new PascalTokenType("-")
  val STAR = new PascalTokenType("*")
  val SLASH = new PascalTokenType("/")
  val COLON_EQUALS = new PascalTokenType(":=")
  val DOT = new PascalTokenType(".")
  val COMMA = new PascalTokenType(",")
  val SEMICOLON = new PascalTokenType(";")
  val COLON = new PascalTokenType(":")
  val QUOTE = new PascalTokenType("'") // TODO: Answer: How is this even use? Does it not case ambiguity between PascalSpecialSymbolToken and PascalStringToken????????
  val EQUALS = new PascalTokenType("=")
  val NOT_EQUALS = new PascalTokenType("<>")
  val LESS_THAN = new PascalTokenType("<")
  val LESS_EQUALS = new PascalTokenType("<=")
  val GREATER_EQUALS = new PascalTokenType(">=")
  val GREATER_THAN = new PascalTokenType(">")
  val LEFT_PAREN = new PascalTokenType("(")
  val RIGHT_PAREN = new PascalTokenType(")")
  val LEFT_BRACKET = new PascalTokenType("[")
  val RIGHT_BRACKET = new PascalTokenType("]")
  val LEFT_BRACE = new PascalTokenType("{")
  val RIGHT_BRACE = new PascalTokenType("}")
  val UP_ARROW = new PascalTokenType("^")
  val DOT_DOT = new PascalTokenType("..")

  val IDENTIFIER = new PascalTokenType()
  val INTEGER = new PascalTokenType()
  val REAL = new PascalTokenType()
  val STRING = new PascalTokenType()
  val ERROR = new PascalTokenType()
  val END_OF_FILE = new PascalTokenType()

  //TODO: Make sure the 2 assignments below actually work.

  // Set of lower-cased Pascal reserved word text strings.
  val RESERVED_WORDS = new mutable.HashMap[String, PascalTokenType]() += (
    AND.getText.toLowerCase -> AND,
    ARRAY.getText.toLowerCase -> ARRAY,
    BEGIN.getText.toLowerCase -> BEGIN,
    CASE.getText.toLowerCase -> CASE,
    CONST.getText.toLowerCase -> CONST,
    DIV.getText.toLowerCase -> DIV,
    DO.getText.toLowerCase -> DO,
    DOWNTO.getText.toLowerCase -> DOWNTO,
    ELSE.getText.toLowerCase -> ELSE,
    END.getText.toLowerCase -> END,
    FILE.getText.toLowerCase -> FILE,
    FOR.getText.toLowerCase -> FOR,
    FUNCTION.getText.toLowerCase -> FUNCTION,
    GOTO.getText.toLowerCase -> GOTO,
    IF.getText.toLowerCase -> IF,
    IN.getText.toLowerCase -> IN,
    LABEL.getText.toLowerCase -> LABEL,
    MOD.getText.toLowerCase -> MOD,
    NIL.getText.toLowerCase -> NIL,
    NOT.getText.toLowerCase -> NOT,
    OF.getText.toLowerCase -> OF,
    OR.getText.toLowerCase -> OR,
    PACKED.getText.toLowerCase -> PACKED,
    PROCEDURE.getText.toLowerCase -> PROCEDURE,
    PROGRAM.getText.toLowerCase -> PROGRAM,
    RECORD.getText.toLowerCase -> RECORD,
    REPEAT.getText.toLowerCase -> REPEAT,
    SET.getText.toLowerCase -> SET,
    THEN.getText.toLowerCase -> THEN,
    TO.getText.toLowerCase -> TO,
    TYPE.getText.toLowerCase -> TYPE,
    UNTIL.getText.toLowerCase -> UNTIL,
    VAR.getText.toLowerCase -> VAR,
    WHILE.getText.toLowerCase -> WHILE,
    WITH.getText.toLowerCase -> WITH
  )

  // HashMap of Pascal special symbols. Each special symbol's text
  // is the key to its Pascal token type.
  val SPECIAL_SYMBOLS = new mutable.HashMap[String, PascalTokenType]() += (
    PLUS.getText -> PLUS,
      MINUS.getText -> MINUS,
      STAR.getText -> STAR,
      SLASH.getText -> SLASH,
      COLON_EQUALS.getText -> COLON_EQUALS,
      DOT.getText -> DOT,
      COMMA.getText -> COMMA,
      SEMICOLON.getText -> SEMICOLON,
      COLON.getText -> COLON,
      QUOTE.getText -> QUOTE,
      EQUALS.getText -> EQUALS,
      NOT_EQUALS.getText -> NOT_EQUALS,
      LESS_THAN.getText -> LESS_THAN,
      LESS_EQUALS.getText -> LESS_EQUALS,
      GREATER_EQUALS.getText -> GREATER_EQUALS,
      GREATER_THAN.getText -> GREATER_THAN,
      LEFT_PAREN.getText -> LEFT_PAREN,
      RIGHT_PAREN.getText -> RIGHT_PAREN,
      LEFT_BRACKET.getText -> LEFT_BRACKET,
      RIGHT_BRACKET.getText -> RIGHT_BRACKET,
      LEFT_BRACE.getText -> LEFT_BRACE,
      RIGHT_BRACE.getText -> RIGHT_BRACE,
      UP_ARROW.getText -> UP_ARROW,
      DOT_DOT.getText -> DOT_DOT
  )

}
