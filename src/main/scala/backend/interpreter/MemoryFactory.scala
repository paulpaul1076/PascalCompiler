package backend.interpreter

import backend.interpreter.memoryimpl._
import intermediate.{SymTab, SymTabEntry}

/**
  * A factory, that creates runtime components.
  */
object MemoryFactory {

  /**
    * Create runtime stack.
    *
    * @return runtime stack.
    */
  def createRuntimeStack(): RuntimeStack = new RuntimeStackImpl()

  /**
    * Create runtime display.
    *
    * @return runtime display.
    */
  def createRuntimeDisplay(): RuntimeDisplay = new RuntimeDisplayImpl()

  /**
    * Create activation record.
    *
    * @param routineId routine id to create activation record from.
    * @return activation record.
    */
  def createActivationRecord(routineId: SymTabEntry): ActivationRecord = new ActivationRecordImpl(routineId)

  /**
    * Create memory map from symbol table.
    *
    * @param symTab symtab to create memory map from.
    * @return memory map.
    */
  def createMemoryMap(symTab: SymTab): MemoryMap = new MemoryMapImpl(symTab)

  /**
    * Create a cell with the value specified.
    *
    * @param value value that this cell will hold.
    * @return cell.
    */
  def createCell(value: Any): Cell = new CellImpl(value)
}
