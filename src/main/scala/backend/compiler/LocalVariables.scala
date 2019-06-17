package backend.compiler

import java.util

class LocalVariables(index: Int) {
  private val reserved = new util.ArrayList[Boolean]()
  for(i <- 0 to index) reserved.add(true)

  /**
    * Reserve a local variable.
    *
    * @return the index of the newly reserved variable.
    */
  def reserve: Int = { // Search for existing but unreserved local variables.
    for (i <- 0 until reserved.size()) {
      if (!reserved.get(i)) {
        reserved.set(i, true)
        return i
      }
    }
    // Reserved a new variable.
    reserved.add(true)
    reserved.size - 1
  }

  /**
    * Release a local variable that's no longer needed.
    *
    * @param index the index of the variable.
    */
  def release(index: Int): Unit = {
    reserved.set(index, false)
  }

  /**
    * Return the count of local variables needed by the method.
    *
    * @return the count.
    */
  def count: Int = reserved.size
}
