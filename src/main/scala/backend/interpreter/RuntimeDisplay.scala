package backend.interpreter

/**
  * Interface for the interpreter's runtime display.
  */
trait RuntimeDisplay {
  /**
    * Get the activation record at a given nesting level.
    *
    * @param nestingLevel the nesting level.
    * @return the activation record.
    */
  def getActivationRecord(nestingLevel: Int): ActivationRecord

  /**
    * Update the display for a call to a routine at a given nesting level.
    *
    * @param nestingLevel the nesting level.
    * @param ar           the activation record for the routine.
    */
  def callUpdate(nestingLevel: Int, ar: ActivationRecord): Unit

  /**
    * Update the display for a return from a routine at a given nesting level.
    *
    * @param nestingLevel the nesting level.
    */
  def returnUpdate(nestingLevel: Int): Unit
}
