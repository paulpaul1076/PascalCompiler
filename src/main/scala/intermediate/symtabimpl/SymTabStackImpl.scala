package intermediate.symtabimpl

import java.util

import intermediate.{SymTab, SymTabEntry, SymTabFactory, SymTabStack}

import scala.util.control.Breaks

/**
  * Implementation of the SymTabStack interface.
  */
class SymTabStackImpl extends util.ArrayList[SymTab] with SymTabStack {

  /**
    * Current scope nesting level.
    */
  private var currentNestingLevel = 0

  /**
    * Entry for the main program id.
    */
  private var programId: SymTabEntry = _

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
    // get(currentNestingLevel).lookup(name)
    var foundEntry: SymTabEntry = null

    // Search the current and enclosing scopes.
    val break = new Breaks

    break.breakable {
      for (i <- currentNestingLevel to 0 by -1) {
        foundEntry = get(i).lookup(name)
        if (foundEntry != null) {
          break.break()
        }
      }
    }

    foundEntry
  }

  /**
    * Look up the entry by name in the local symbol table.
    *
    * @param name the name of the entry.
    * @return SymTabEntry instance.
    */
  override def lookupLocal(name: String): SymTabEntry = {
    get(currentNestingLevel).lookup(name)
  }

  /**
    * Getter.
    *
    * @return the symbol table entry for the main program identifier.
    */
  override def getProgramId(): SymTabEntry = programId

  /**
    * Setter.
    *
    * @param entry the symbol table entry for the main program identifier.
    */
  override def setProgramId(entry: SymTabEntry): Unit = {
    this.programId = entry
  }

  /**
    * Push a new symbol table onto the stack.
    *
    * @return the pushed symbol table.
    */
  override def push(): SymTab = {
    currentNestingLevel += 1
    val symTab = SymTabFactory.createSymTab(currentNestingLevel)
    add(symTab)

    symTab
  }

  /**
    * Push a symbol table onto the stack.
    *
    * @param symTab the symbol table to push.
    * @return the pushed symbol table.
    */
  override def push(symTab: SymTab): SymTab = {
    currentNestingLevel += 1
    add(symTab)

    symTab
  }

  /**
    * Pop a symbol table off the stack.
    *
    * @return the popped symbol table.
    */
  override def pop(): SymTab = {
    val symTab = get(currentNestingLevel)
    remove(currentNestingLevel)
    currentNestingLevel -= 1

    symTab
  }
}
