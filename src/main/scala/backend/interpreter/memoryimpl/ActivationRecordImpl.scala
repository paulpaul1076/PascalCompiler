package backend.interpreter.memoryimpl

import java.util

import backend.interpreter.{ActivationRecord, Cell, MemoryFactory}
import intermediate.symtabimpl.SymTabKeyImpl
import intermediate.{SymTab, SymTabEntry}


/**
  * The runtime activation record.
  */
class ActivationRecordImpl(routineIdd: SymTabEntry) extends ActivationRecord {

  private val routineId = routineIdd
  private val symTab = routineId.getAttribute(SymTabKeyImpl.ROUTINE_SYMTAB).asInstanceOf[SymTab]
  private val nestingLevel = symTab.getNestingLevel
  private val memoryMap = MemoryFactory.createMemoryMap(symTab)
  private var link: ActivationRecord = _

  /**
    * Getter.
    *
    * @return the symbol table entry of the routine's name.
    */
  override def getRoutineId: SymTabEntry = routineId

  /**
    * Return the memory cell for the given name from the memory map.
    *
    * @param name the name
    * @return the cell.
    */
  override def getCell(name: String): Cell = memoryMap.getCell(name)

  /**
    *
    * @return the list of all the names in the memory map.
    */
  override def getAllNames: util.ArrayList[String] = memoryMap.getAllNames

  /**
    * Getter.
    *
    * @return the scope nesting level.
    */
  override def getNestingLevel: Int = nestingLevel

  /**
    *
    * @return the activation record to which this record is dynamically linked.
    */
  override def linkedTo(): ActivationRecord = link

  /**
    * Make a dynamic link from this activation record to another one.
    *
    * @param ar the activation record to link to.
    * @return this activation record.
    */
  override def makeLinkTo(ar: ActivationRecord): ActivationRecord = {
    link = ar
    this
  }
}
