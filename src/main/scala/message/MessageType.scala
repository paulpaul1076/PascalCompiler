package message

object MessageType extends Enumeration {
  val SOURCE_LINE = Value
  val SYNTAX_ERROR = Value
  val PARSER_SUMMARY = Value
  val INTERPRETER_SUMMARY = Value
  val COMPILER_SUMMARY = Value
  val MISCELLANEOUS = Value
  val TOKEN = Value
  val ASSIGN = Value
  val FETCH = Value
  val BREAKPOINT = Value
  val RUNTIME_ERROR = Value
  val CALL = Value
  val RETURN = Value
}
