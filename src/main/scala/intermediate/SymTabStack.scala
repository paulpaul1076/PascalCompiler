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

  /**
    * Setter.
    *
    * @param entry the symbol table entry for the main program identifier.
    */
  def setProgramId(entry: SymTabEntry): Unit

  /**
    * Getter.
    *
    * @return the symbol table entry for the main program identifier.
    */
  def getProgramId(): SymTabEntry

  /**
    * Push a new symbol table onto the stack.
    *
    * @return the pushed symbol table.
    */
  def push(): SymTab

  /**
    * Push a symbol table onto the stack.
    *
    * @param symTab the symbol table to push.
    * @return the pushed symbol table.
    */
  def push(symTab: SymTab): SymTab

  /**
    * Pop a symbol table off the stack.
    *
    * @return the popped symbol table.
    */
  def pop(): SymTab
}
