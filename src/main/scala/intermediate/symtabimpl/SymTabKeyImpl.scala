package intermediate.symtabimpl

import intermediate.SymTabKey

/**
  * Attribute keys for a symbol table entry.
  */
class SymTabKeyImpl extends SymTabKey {

}

/**
  * "Enum" for all kinds of symbol table keys.
  */
object SymTabKeyImpl {
  // Constant.
  val CONSTANT_VALUE = new SymTabKeyImpl

  // Procedure or function.
  val ROUTINE_CODE = new SymTabKeyImpl
  val ROUTINE_SYMTAB = new SymTabKeyImpl
  val ROUTINE_ICODE = new SymTabKeyImpl
  val ROUTINE_PARMS = new SymTabKeyImpl
  val ROUTINE_ROUTINES = new SymTabKeyImpl

  // Variable or record field value.
  val DATA_VALUE = new SymTabKeyImpl

  // Local variables array slot numbers
  val SLOT = new SymTabKeyImpl
  val WRAP_SLOT = new SymTabKeyImpl
}