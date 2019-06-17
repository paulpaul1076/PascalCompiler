package intermediate.symtabimpl

import java.util

import intermediate.{SymTab, SymTabEntry, SymTabFactory}

class SymTabImpl(nl: Int) extends util.TreeMap[String, SymTabEntry] with SymTab {

  var nestingLevel = nl
  var slotNumber = -1
  var maxSlotNumber = 0

  /**
    * Getter.
    *
    * @return nesting level.
    */
  override def getNestingLevel: Int = nestingLevel

  /**
    * Create and enter a new entry into the symbol table.
    *
    * @param name the name of the entry.
    * @return the new entry's instance.
    */
  override def enter(name: String): SymTabEntry = {
    val entry = SymTabFactory.createSymTabEntry(name, this)
    put(name, entry)
    entry
  }

  /**
    * Look up an existing symbol table entry.
    *
    * @param name the name of the entry.
    * @return the entry, or null if it does not exist.
    */
  override def lookup(name: String): SymTabEntry = {
    get(name)
  }

  /**
    * A sorted by name list of entries.
    *
    * @return A sorted by name list of entries.
    */
  override def sortedEntries: util.ArrayList[SymTabEntry] = {
    val result = new util.ArrayList[SymTabEntry]()
    /*for (entry <- this.values())*/ this.values().stream().forEach(entry => {
      result.add(entry)
    })
    result
  }

  override def nextSlotNumber(): Int = {
    slotNumber += 1
    maxSlotNumber = slotNumber
    return slotNumber
  }

  override def getMaxSlotNumber(): Int = maxSlotNumber
}
