package intermediate

import intermediate.symtabimpl.{SymTabEntryImpl, SymTabImpl, SymTabStackImpl}

/**
  * A factory for creating objects that implements the symbol table.
  */
object SymTabFactory {
  /**
    * Create and return a symbol table stack implementation.
    *
    * @return the symbol table implementation.
    */
  def createSymTabStack(): SymTabStack = {
    new SymTabStackImpl
  }

  /**
    * Create and return a symbol table implementation.
    *
    * @param nestingLevel the nesting level.
    * @return the symbol table implementation.
    */
  def createSymTab(nestingLevel: Int): SymTab = {
    new SymTabImpl(nestingLevel)
  }

  /**
    * Create and return a symbol table entry implementation.
    *
    * @param name   the identifier name.
    * @param symTab the symbol table that contains the entry.
    * @return the symbol table entry implementation.
    */
  def createSymTabEntry(name: String, symTab: SymTab): SymTabEntry = {
    new SymTabEntryImpl(name, symTab)
  }
}
