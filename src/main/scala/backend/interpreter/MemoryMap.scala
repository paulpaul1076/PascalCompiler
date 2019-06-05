package backend.interpreter

import java.util

/**
  * Interface for the interpreter's runtime memory map.
  */
trait MemoryMap {
  /**
    * Return the memory cell with the given name.
    *
    * @param name the name.
    * @return the cell.
    */
  def getCell(name: String): Cell

  /**
    *
    * @return the list of all the names.
    */
  def getAllNames: util.ArrayList[String]
}
