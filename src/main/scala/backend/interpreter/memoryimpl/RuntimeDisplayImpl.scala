package backend.interpreter.memoryimpl

import java.util

import backend.interpreter.{ActivationRecord, RuntimeDisplay}

class RuntimeDisplayImpl extends util.ArrayList[ActivationRecord] with RuntimeDisplay {

  add(null) // add a dummy element 0 (never used)

  /**
    * Get the activation record at a given nesting level.
    *
    * @param nestingLevel the nesting level.
    * @return the activation record.
    */
  override def getActivationRecord(nestingLevel: Int): ActivationRecord = get(nestingLevel)

  /**
    * Update the display for a call to a routine at a given nesting level.
    *
    * @param nestingLevel the nesting level.
    * @param ar           the activation record for the routine.
    */
  override def callUpdate(nestingLevel: Int, ar: ActivationRecord): Unit = {
    if (nestingLevel >= size()) {
      add(ar)
    }
    // Existing nesting level: Set at the specified level.
    else {
      val prevAr = get(nestingLevel)
      set(nestingLevel, ar.makeLinkTo(prevAr))
    }
  }

  /**
    * Update the display for a return from a routine at a given nesting level.
    *
    * @param nestingLevel the nesting level.
    */
  override def returnUpdate(nestingLevel: Int): Unit = {
    val topIndex = size() - 1
    val ar = get(nestingLevel) // AR about to be popped off
    val prevAr = ar.linkedTo() // previous AR it points to

    // Point the element at that nesting level to the previous activation record.
    if (prevAr != null) {
      set(nestingLevel, prevAr)
    }
    // The top element has become null, so remove it.
    else if (nestingLevel == topIndex) {
      remove(topIndex)
    }
  }
}
