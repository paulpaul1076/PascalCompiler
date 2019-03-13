package frontend.pascal

/**
 * PascalErrorCode.
 */
class PascalErrorCode(val errorCode: Int, val message: String) extends Enumeration {

  def this(message: String) {
    this(0, message)
  }

  val ALREADY_FORWARDED = Value("Already specified in FORWARD")


}
