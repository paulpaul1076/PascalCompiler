package intermediate

/**
  * The interface for the symbol table stack.
  */
trait SymTabStack {
  /**
    * Getter for the current nesting level.
    *
    * @return current nesting level.
    */
  def getCurrentNestingLevel: Int

  /**
    * Gets the symbol table entry that is at the top of the stack.
    *
    * @return symbol table entry that is at the top of the stack.
    */
  def getLocalSymTab: SymTab

  /**
    * Add a symbol table entry to the symbol table that is at the top of the stack.
    *
    * @param name name of the entry
    * @return SymTabEntry instance.
    */
  def enterLocal(name: String): SymTabEntry

  /**
    * Look up the entry by name in the local symbol table.
    *
    * @param name the name of the entry.
    * @return SymTabEntry instance.
    */
  def lookupLocal(name: String): SymTabEntry

  /**
    * Look up an entry in ALL symbol tables.
    *
    * @param name entry's name.
    * @return SymTabEntry instance.
    */
  def lookup(name: String): SymTabEntry
}
