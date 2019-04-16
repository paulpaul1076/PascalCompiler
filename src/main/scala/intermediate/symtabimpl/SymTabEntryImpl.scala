package intermediate.symtabimpl

import java.util

import intermediate._

/**
  * Implementation of the SymTabEntry interface.
  *
  * @param name   name of the entry.
  * @param symTab the symbol table that contains this entry.
  */
class SymTabEntryImpl(val name: String, val symTab: SymTab) extends util.HashMap[SymTabKey, Any] with SymTabEntry {

  private val lineNumbers = new util.ArrayList[Int]()
  private var definition: Definition = _
  private var typeSpec: TypeSpec = _

  override def equals(o: Any): Boolean = {
    if (o.getClass == classOf[SymTabEntryImpl]) {
      val other = o.asInstanceOf[SymTabEntryImpl]
      this.name == other.name
    } else {
      false
    }
  }

  override def hashCode(): Int = {
    name.hashCode()
  }

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

  /**
    * Getter.
    *
    * @return definition of the entry.
    */
  override def getDefinition: Definition = definition

  override def setDefinition(definition: Definition): Unit = this.definition = definition

  /**
    * Getter.
    *
    * @return type spec.
    */
  override def getTypeSpec: TypeSpec = typeSpec

  /**
    * Setter.
    *
    * @param typeSpec type spec.
    */
  override def setTypeSpec(typeSpec: TypeSpec): Unit = this.typeSpec = typeSpec
}
