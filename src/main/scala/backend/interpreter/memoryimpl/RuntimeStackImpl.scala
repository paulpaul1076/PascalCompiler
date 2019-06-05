package backend.interpreter.memoryimpl

import java.util

import backend.interpreter.{ActivationRecord, MemoryFactory, RuntimeDisplay, RuntimeStack}

class RuntimeStackImpl extends util.ArrayList[ActivationRecord] with RuntimeStack {

  private val display: RuntimeDisplay = MemoryFactory.createRuntimeDisplay() // runtime display

  /**
    * @param nestingLevel nesting level.
    * @return get topmost activation record without popping.
    */
  override def getTopmost(nestingLevel: Int): ActivationRecord = display.getActivationRecord(nestingLevel)

  /**
    * push an activation record onto the stack.
    *
    * @param ar activation record to push.
    */
  override def push(ar: ActivationRecord): Unit = {
    val nestingLevel = ar.getNestingLevel
    add(ar)

    display.callUpdate(nestingLevel, ar)
  }

  /**
    *
    * @return Pop an activation record off the stack.
    */
  override def pop(): ActivationRecord = {
    display.returnUpdate(currentNestingLevel)
    remove(size() - 1)
  }

  /**
    *
    * @return the nesting level of the activation record that's at the top of the runtime stack.
    */
  override def currentNestingLevel: Int = {
    val topIndex = size() - 1

    if (topIndex >= 0) get(topIndex).getNestingLevel else -1
  }

  /**
    *
    * @return an array list of the activation records on the stack.
    */
  override def records(): util.ArrayList[ActivationRecord] = this
}
