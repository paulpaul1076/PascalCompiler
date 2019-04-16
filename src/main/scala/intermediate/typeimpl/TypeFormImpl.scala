package intermediate.typeimpl

import intermediate.TypeForm

class TypeFormImpl(val name: String) extends TypeForm {
  override def toString: String = name.toLowerCase()
}

/**
  * Enumeration, containing all types of type forms.
  */
object TypeFormImpl {
  val SCALAR = new TypeFormImpl("SCALAR")
  val ENUMERATION = new TypeFormImpl("ENUMERATION")
  val SUBRANGE = new TypeFormImpl("SUBRANGE")
  val ARRAY = new TypeFormImpl("ARRAY")
  val RECORD = new TypeFormImpl("RECORD")
}
