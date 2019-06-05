package backend.interpreter.memoryimpl

import backend.interpreter.Cell

/**
  * The interpreter's runtime memory cell.
  */
class CellImpl(newValue: Any) extends Cell {
  private var value: Any = newValue

  /**
    *
    * @return the value in the cell.
    */
  override def getValue: Any = value

  /**
    * Set a new value into the cell.
    *
    * @param newValue the new value.
    */
  override def setValue(newValue: Any): Unit = {
    this.value = newValue
  }
}
