package backend.interpreter.memoryimpl

import java.util

import backend.interpreter.{Cell, MemoryFactory, MemoryMap}
import intermediate.symtabimpl.DefinitionImpl
import intermediate.typeimpl.{TypeFormImpl, TypeKeyImpl}
import intermediate.{SymTab, TypeSpec}

import scala.collection.JavaConverters._

/**
  * This is like a symbol table for activation records.
  * @param symTab symbol table to create this memory map from.
  */
class MemoryMapImpl(symTab: SymTab) extends util.HashMap[String, Cell] with MemoryMap {

  private val entries = symTab.sortedEntries

  // CONSTRUCTOR
  {
    // loop for each entry in the symbol table.
    for (entry <- entries.asScala) {
      val defn = entry.getDefinition

      // Not a VAR parameter: Allocate cells for the data type
      //                      in the hashmap.
      if ((defn == DefinitionImpl.VARIABLE) ||
        (defn == DefinitionImpl.FUNCTION) ||
        (defn == DefinitionImpl.VALUE_PARM) ||
        (defn == DefinitionImpl.FIELD)) {

        val name = entry.getName
        val `type` = entry.getTypeSpec
        put(name, MemoryFactory.createCell(allocateCellValue(`type`)))
      }
      // VAR parameter: Allocate a single cell to hold a reference
      //                in the hashmap
      else if (defn == DefinitionImpl.VAR_PARM) {
        val name = entry.getName
        put(name, MemoryFactory.createCell(null))
      }
    }
  }

  /**
    * Return the memory cell with the given name.
    *
    * @param name the name.
    * @return the cell.
    */
  override def getCell(name: String): Cell = get(name)


  /**
    *
    * @return the list of all the names.
    */
  override def getAllNames: util.ArrayList[String] = {
    val list = new util.ArrayList[String]()
    val it = keySet().iterator()
    while (it.hasNext) {
      list.add(it.next())
    }
    list
  }

  /**
    * Make an allocation for a value of a given data type for a memory cell.
    *
    * @param `type` the data type.
    * @return the allocation.
    */
  private def allocateCellValue(`type`: TypeSpec): Any = {
    val form = `type`.getForm

    form.asInstanceOf[TypeFormImpl] match {
      case TypeFormImpl.ARRAY => allocateArrayCells(`type`)
      case TypeFormImpl.RECORD => allocateRecordMap(`type`)
      case _ => null // uninitialized scalar value
    }
  }

  /**
    * Allocate the memory cells of an array.
    *
    * @param `type` the array type.
    * @return the allocation.
    */
  private def allocateArrayCells(`type`: TypeSpec): Array[Cell] = {
    val elmtCount = `type`.getAttribute(TypeKeyImpl.ARRAY_ELEMENT_COUNT).asInstanceOf[Int] // TODO: Int or Integer?
    val elmtType = `type`.getAttribute(TypeKeyImpl.ARRAY_ELEMENT_TYPE).asInstanceOf[TypeSpec]
    val allocation = new Array[Cell](elmtCount)

    for (i <- 0 until elmtCount) {
      allocation(i) = MemoryFactory.createCell(allocateCellValue(elmtType))
    }

    allocation
  }

  /**
    * Allocate the memory map for a record.
    *
    * @param `type` the record type.
    * @return the allocation.
    */
  private def allocateRecordMap(`type`: TypeSpec): MemoryMap = {
    val symTab = `type`.getAttribute(TypeKeyImpl.RECORD_SYMTAB).asInstanceOf[SymTab]
    val memoryMap = MemoryFactory.createMemoryMap(symTab)

    memoryMap
  }
}
