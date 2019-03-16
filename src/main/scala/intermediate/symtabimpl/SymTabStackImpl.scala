package intermediate.symtabimpl

import java.util

import intermediate.{SymTab, SymTabEntry, SymTabFactory, SymTabStack}

/**
  * Implementation of the SymTabStack interface.
  */
class SymTabStackImpl extends util.ArrayList[SymTab] with SymTabStack {

  /**
    * Current scope nesting level.
    */
  private var currentNestingLevel = 0

  // Constructor stuff.
  add(SymTabFactory.createSymTab(currentNestingLevel))

  /**
    * Getter for the current nesting level.
    *
    * @return current nesting level.
    */
  override def getCurrentNestingLevel: Int = currentNestingLevel

  /**
    * Gets the symbol table entry that is at the top of the stack.
    *
    * @return symbol table entry that is at the top of the stack.
    */
  override def getLocalSymTab: SymTab = get(currentNestingLevel)

  /**
    * Add a symbol table entry to the symbol table that is at the top of the stack.
    *
    * @param name name of the entry
    * @return SymTabEntry instance.
    */
  override def enterLocal(name: String): SymTabEntry = get(currentNestingLevel).enter(name)

  /**
    * Look up an entry in ALL symbol tables.
    *
    * @param name entry's name.
    * @return SymTabEntry instance.
    */
  override def lookup(name: String): SymTabEntry = {
    lookupLocal(name)
  }

  /**
    * Look up the entry by name in the local symbol table.
    *
    * @param name the name of the entry.
    * @return SymTabEntry instance.
    */
  override def lookupLocal(name: String): SymTabEntry = get(currentNestingLevel).lookup(name)
}
