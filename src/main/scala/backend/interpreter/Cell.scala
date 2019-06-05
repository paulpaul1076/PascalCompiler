package backend.interpreter

/**
  * Interface for the interpreter's runtime memory cell.
  */
trait Cell {
  /**
    * Set a new value into the cell.
    *
    * @param newValue the new value.
    */
  def setValue(newValue: Any): Unit

  /**
    *
    * @return the value in the cell.
    */
  def getValue: Any
}
