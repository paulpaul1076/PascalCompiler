package backend.interpreter

import java.util

/**
  * Interface for interpreter's runtime stack.
  */
trait RuntimeStack {
  /**
    * @param nestingLevel nesting level of Symentry corresponding of this activation record
    * @return get topmost activation record without popping.
    */
  def getTopmost(nestingLevel: Int): ActivationRecord

  /**
    *
    * @return the nesting level of the activation record that's at the top of the runtime stack.
    */
  def currentNestingLevel: Int

  /**
    * push an activation record onto the stack.
    *
    * @param ar activation record to push.
    */
  def push(ar: ActivationRecord): Unit

  /**
    *
    * @return Pop an activation record off the stack.
    */
  def pop(): ActivationRecord

  /**
    *
    * @return an array list of the activation records on the stack.
    */
  def records(): util.ArrayList[ActivationRecord]
}
