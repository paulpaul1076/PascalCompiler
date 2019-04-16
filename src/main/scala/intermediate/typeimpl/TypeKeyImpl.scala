package intermediate.typeimpl

import intermediate.TypeKey

class TypeKeyImpl extends TypeKey {

}

/**
  * Types of information needed for parse to extract each type form.
  */
object TypeKeyImpl {
  // Enumeration
  val ENUMERATION_CONSTANTS = new TypeKeyImpl

  // Subrange
  val SUBRANGE_BASE_TYPE = new TypeKeyImpl
  val SUBRANGE_MIN_VALUE = new TypeKeyImpl
  val SUBRANGE_MAX_VALUE = new TypeKeyImpl

  // Array
  val ARRAY_INDEX_TYPE = new TypeKeyImpl
  val ARRAY_ELEMENT_TYPE = new TypeKeyImpl
  val ARRAY_ELEMENT_COUNT = new TypeKeyImpl

  // Record
  val RECORD_SYMTAB = new TypeKeyImpl
}
