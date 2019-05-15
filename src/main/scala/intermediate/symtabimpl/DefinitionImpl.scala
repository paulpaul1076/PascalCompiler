package intermediate.symtabimpl

import intermediate.Definition

/**
  * Implementation of the definition interface.
  *
  * @param text text of the definition.
  */
class DefinitionImpl(text: String) extends Definition {
  private var text1: String = text.toLowerCase()

  override def toString: String = getText.toUpperCase

  /**
    * Getter.
    *
    * @return String the text of the definition.
    */
  override def getText: String = text1
}

object DefinitionImpl {
  val CONSTANT = new DefinitionImpl("CONSTANT")
  val ENUMERATION_CONSTANT = new DefinitionImpl("enumeration constant")
  val TYPE = new DefinitionImpl("TYPE")
  val VARIABLE = new DefinitionImpl("VARIABLE")
  val FIELD = new DefinitionImpl("record field")
  val VALUE_PARM = new DefinitionImpl("value parameter")
  val VAR_PARM = new DefinitionImpl("VAR parameter")
  val PROGRAM_PARM = new DefinitionImpl("program parameter")
  val PROGRAM = new DefinitionImpl("PROGRAM")
  val PROCEDURE = new DefinitionImpl("PROCEDURE")
  val FUNCTION = new DefinitionImpl("FUNCTION")
  val UNDEFINED = new DefinitionImpl("UNDEFINED")
}
