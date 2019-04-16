package intermediate

import java.util

/**
  * The interface for the symbol table entry.
  */
trait SymTabEntry {
  /**
    * Getter.
    *
    * @return name of the entry.
    */
  def getName: String

  /**
    * Getter.
    *
    * @return symbol table instance that has this entry.
    */
  def getSymTab: SymTab

  /**
    * Append a source line number to the entry.
    *
    * @param lineNumber the line number to append.
    */
  def appendLineNumber(lineNumber: Int): Unit

  /**
    * Set an attribute of the entry.
    *
    * @param key   key the attribute key.
    * @param value value the attribute value.
    */
  def setAttribute(key: SymTabKey, value: Any)

  /**
    * Get the value of an attribute of the entry.
    *
    * @param key the attribute key.
    * @return the attribute value.
    */
  def getAttribute(key: SymTabKey): Any

  /**
    * Getter.
    *
    * @return the list of source line numbers.
    */
  def getLineNumbers: util.ArrayList[Int]

  def setDefinition(definition: Definition): Unit

  def getDefinition: Definition

  def setTypeSpec(typeSpec: TypeSpec): Unit

  def getTypeSpec: TypeSpec
}
