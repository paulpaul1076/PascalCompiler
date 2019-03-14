package frontend.pascal

/**
 * PascalErrorCode.
 */
class PascalErrorCode(val status: Int, val message: String) {

  /**
   * A secondary constructor.
   * @param message message of the error.
   */
  def this(message: String) {
    this(0, message)
  }

  /**
   * Getter of status.
   * @return status.
   */
  def getStatus : Int = status

  /**
   * To string method that prints out the error message.
   * @return error message.
   */
  override def toString: String = message
}

/**
 * Error code values.
 */
object PascalErrorCode {
  val ALREADY_FORWARDED = new PascalErrorCode("Already specified in FORWARD")
  val IDENTIFIER_REDEFINED = new PascalErrorCode("Redefined identifier")
  val IDENTIFIER_UNDEFINED = new PascalErrorCode("Undefined identifier")
  val INCOMPATIBLE_ASSIGNMENT = new PascalErrorCode("Incompatible assignment")
  val INCOMPATIBLE_TYPES = new PascalErrorCode("Incompatible types")
  val INVALID_ASSIGNMENT = new PascalErrorCode("Invalid assignment statement")
  val INVALID_CHARACTER = new PascalErrorCode("Invalid character")
  val INVALID_CONSTANT = new PascalErrorCode("Invalid constant")
  val INVALID_EXPONENT = new PascalErrorCode("Invalid exponent")
  val INVALID_EXPRESSION = new PascalErrorCode("Invalid expression")
  val INVALID_FIELD = new PascalErrorCode("Invalid field")
  val INVALID_FRACTION = new PascalErrorCode("Invalid fraction")
  val INVALID_IDENTIFIER_USAGE = new PascalErrorCode("Invalid identifier usage")
  val INVALID_INDEX_TYPE = new PascalErrorCode("Invalid index type")
  val INVALID_NUMBER = new PascalErrorCode("Invalid number")
  val INVALID_STATEMENT = new PascalErrorCode("Invalid statement")
  val INVALID_SUBRANGE_TYPE = new PascalErrorCode("Invalid subrange type")
  val INVALID_TARGET = new PascalErrorCode("Invalid assignment target")
  val INVALID_TYPE = new PascalErrorCode("Invalid type")
  val INVALID_VAR_PARM = new PascalErrorCode("Invalid VAR parameter")
  val MIN_GT_MAX = new PascalErrorCode("Min limit greater than max limit")
  val MISSING_BEGIN = new PascalErrorCode("Missing BEGIN")
  val MISSING_COLON = new PascalErrorCode("Missing :")
  val MISSING_COLON_EQUALS = new PascalErrorCode("Missing :=")
  val MISSING_COMMA = new PascalErrorCode("Missing ,")
  val MISSING_CONSTANT = new PascalErrorCode("Missing constant")
  val MISSING_DO = new PascalErrorCode("Missing DO")
  val MISSING_DOT_DOT = new PascalErrorCode("Missing ..")
  val MISSING_END = new PascalErrorCode("Missing END")
  val MISSING_EQUALS = new PascalErrorCode("Missing =")
  val MISSING_FOR_CONTROL = new PascalErrorCode("Missing FOR control variable")
  val MISSING_IDENTIFIER = new PascalErrorCode("Missing identifier")
  val MISSING_LEFT_BRACKET = new PascalErrorCode("Missing [")
  val MISSING_OF = new PascalErrorCode("Missing OF")
  val MISSING_PERIOD = new PascalErrorCode("Missing .")
  val MISSING_PROGRAM = new PascalErrorCode("Missing PROGRAM")
  val MISSING_RIGHT_BRACKET = new PascalErrorCode("Missing ]")
  val MISSING_RIGHT_PAREN = new PascalErrorCode("Missing )")
  val MISSING_SEMICOLON = new PascalErrorCode("Missing ;")
  val MISSING_THEN = new PascalErrorCode("Missing THEN")
  val MISSING_TO_DOWNTO = new PascalErrorCode("Missing TO or DOWNTO")
  val MISSING_UNTIL = new PascalErrorCode("Missing UNTIL")
  val MISSING_VARIABLE = new PascalErrorCode("Missing variable")
  val CASE_CONSTANT_REUSED = new PascalErrorCode("CASE constant reused")
  val NOT_CONSTANT_IDENTIFIER = new PascalErrorCode("Not a constant identifier")
  val NOT_RECORD_VARIABLE = new PascalErrorCode("Not a record variable")
  val NOT_TYPE_IDENTIFIER = new PascalErrorCode("Not a type identifier")
  val RANGE_INTEGER = new PascalErrorCode("Integer literal out of range")
  val RANGE_REAL = new PascalErrorCode("Read literal out of range")
  val STACK_OVERFLOW = new PascalErrorCode("Stack overflow")
  val TOO_MANY_LEVELS = new PascalErrorCode("Nesting level too deep")
  val TOO_MANY_SUBSCRIPTS = new PascalErrorCode("Too many subscripts")
  val UNEXPECTED_EOF = new PascalErrorCode("Unexpected end of file")
  val UNEXPECTED_TOKEN = new PascalErrorCode("Unexpected token")
  val UNIMPLEMENTED = new PascalErrorCode("Unimplemented feature")
  val UNRECOGNIZABLE = new PascalErrorCode("Unrecognizable input")
  val WRONG_NUMBER_OF_PARMS = new PascalErrorCode("Wrong number of actual parameters")

  // Fatal errors
  val IO_ERROR = new PascalErrorCode(-101, "Object I/O error")
  val TOO_MANY_ERRORS = new PascalErrorCode(-102, "Too many syntax errors")


}
