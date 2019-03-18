package intermediate.symtabimpl

import java.util

import intermediate.{SymTab, SymTabEntry, SymTabKey}

/**
  * Implementation of the SymTabEntry interface.
  *
  * @param name   name of the entry.
  * @param symTab the symbol table that contains this entry.
  */
class SymTabEntryImpl(val name: String, val symTab: SymTab) extends util.HashMap[SymTabKey, Any] with SymTabEntry {

  private val lineNumbers = new util.ArrayList[Int]()

  /**
    * Getter.
    *
    * @return name of the entry.
    */
  override def getName: String = {
    name
  }

  /**
    * Getter.
    *
    * @return symbol table instance that has this entry.
    */
  override def getSymTab: SymTab = {
    symTab
  }

  /**
    * Append a source line number to the entry.
    *
    * @param lineNumber the line number to append.
    */
  override def appendLineNumber(lineNumber: Int): Unit = {
    lineNumbers.add(lineNumber)
  }

  /**
    * Set an attribute of the entry.
    *
    * @param key   key the attribute key.
    * @param value value the attribute value.
    */
  override def setAttribute(key: SymTabKey, value: Any): Unit = {
    put(key, value)
  }

  /**
    * Get the value of an attribute of the entry.
    *
    * @param key the attribute key.
    * @return the attribute value.
    */
  override def getAttribute(key: SymTabKey): Any = {
    get(key)
  }

  /**
    * Getter.
    *
    * @return the list of source line numbers.
    */
  override def getLineNumbers: util.ArrayList[Int] = {
    lineNumbers
  }
}
