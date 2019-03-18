package intermediate

import java.util

/**
  * The framework interface that represents the symbol table.
  */
trait SymTab {
  /**
    * Getter.
    *
    * @return nesting level.
    */
  def getNestingLevel: Int

  /**
    * Create and enter a new entry into the symbol table.
    *
    * @param name the name of the entry.
    * @return the new entry's instance.
    */
  def enter(name: String): SymTabEntry

  /**
    * Look up an existing symbol table entry.
    *
    * @param name the name of the entry.
    * @return the entry, or null if it does not exist.
    */
  def lookup(name: String): SymTabEntry

  /**
    * A sorted by name list of entries.
    *
    * @return A sorted by name list of entries.
    */
  def sortedEntries: util.ArrayList[SymTabEntry]
}
