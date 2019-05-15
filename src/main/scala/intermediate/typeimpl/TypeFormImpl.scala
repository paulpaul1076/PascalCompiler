package intermediate.typeimpl

import intermediate.TypeForm

class TypeFormImpl(val name: String) extends TypeForm {
  override def toString: String = name.toLowerCase()

  override def hashCode(): Int = name.hashCode

  override def equals(obj: Any): Boolean = {
    if (obj.isInstanceOf[TypeFormImpl]) {
      val other = obj.asInstanceOf[TypeFormImpl]
      return this.name == other.name
    }
    false
  }
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
