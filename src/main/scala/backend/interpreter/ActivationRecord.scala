package backend.interpreter

import java.util

import intermediate.SymTabEntry

/**
  * Interface for the interpreter's runtime activation record.
  */
trait ActivationRecord {
  /**
    * Getter.
    *
    * @return the symbol table entry of the routine's name.
    */
  def getRoutineId: SymTabEntry

  /**
    * Return the memory cell for the given name from the memory map.
    *
    * @param name the name
    * @return the cell.
    */
  def getCell(name: String): Cell

  /**
    *
    * @return the list of all the names in the memory map.
    */
  def getAllNames: util.ArrayList[String]

  /**
    * Getter.
    *
    * @return the scope nesting level.
    */
  def getNestingLevel: Int

  /**
    *
    * @return the activation record to which this record is dynamically linked.
    */
  def linkedTo(): ActivationRecord

  /**
    * Make a dynamic link from this activation record to another one.
    *
    * @param ar the activation record to link to.
    * @return this activation record.
    */
  def makeLinkTo(ar: ActivationRecord): ActivationRecord
}
