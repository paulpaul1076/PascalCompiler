package backend.interpreter

/**
 * Class for runtime error codes.
 *
 * @param message message of the error code.
 */
class RuntimeErrorCode(val message: String) {
  override def toString: String = message
}

/**
 * Runtime error codes.
 */
object RuntimeErrorCode {
  val UNINITIALIZED_VALUE = new RuntimeErrorCode("Uninitialized value")
  val VALUE_RANGE = new RuntimeErrorCode("Value out of range")
  val INVALID_CASE_EXPRESSION_VALUE = new RuntimeErrorCode("Invalid CASE expression value")
  val DIVISION_BY_ZERO = new RuntimeErrorCode("Division by zero")
  val INVALID_STANDARD_FUNCTION_ARGUMENT = new RuntimeErrorCode("Invalid standard function argument")
  val INVALID_INPUT = new RuntimeErrorCode("Invalid input")
  val STACK_OVERFLOW = new RuntimeErrorCode("Runtime stack overflow")
  val UNIMPLEMENTED_FEATURE = new RuntimeErrorCode("Unimplemented runtime feature")
}