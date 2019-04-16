package intermediate.typeimpl

import java.util

import intermediate.symtabimpl.Predefined
import intermediate.{SymTabEntry, TypeForm, TypeKey, TypeSpec}

/**
  * Symbol table for types.
  *
  * @param form       form of the type (ARRAY, ENUMERATION, RANGE, RECORD).
  * @param identifier identifier from the symbol table.
  */
class TypeSpecImpl private(val form: TypeForm, var identifier: SymTabEntry) extends util.HashMap[TypeKey, Any] with TypeSpec {

  // Had to define this, because comparing newed objects with each other would always return "true".
  override def equals(o: Any): Boolean = {
    if (o.getClass == classOf[TypeSpecImpl]) {
      val other = o.asInstanceOf[TypeSpecImpl]
      this.form == other.form && this.identifier == other.identifier
    } else {
      false
    }
  }

  override def hashCode(): Int = {
    form.hashCode() ^ identifier.hashCode()
  }

  /**
    * Constructor.
    *
    * @param value the type form.
    */
  def this(value: String) {
    this(TypeFormImpl.ARRAY)

    val indexType = new TypeSpecImpl(TypeFormImpl.SUBRANGE)

    indexType.setAttribute(TypeKeyImpl.SUBRANGE_BASE_TYPE, Predefined.integerType)
    indexType.setAttribute(TypeKeyImpl.SUBRANGE_MIN_VALUE, 1)
    indexType.setAttribute(TypeKeyImpl.SUBRANGE_MAX_VALUE, value.length)

    setAttribute(TypeKeyImpl.ARRAY_INDEX_TYPE, indexType)
    setAttribute(TypeKeyImpl.ARRAY_ELEMENT_TYPE, Predefined.charType)
    setAttribute(TypeKeyImpl.ARRAY_ELEMENT_COUNT, value.length)
  }

  def this(form: TypeForm) {
    this(form, null)
  }

  /**
    * Type attributes.
    *
    * @param key   attr key.
    * @param value attr value.
    */
  override def setAttribute(key: TypeKey, value: Any): Unit = this.put(key, value)

  /**
    * Getter.
    *
    * @return the type form (such as scalar, array, or record).
    */
  override def getForm: TypeForm = form

  /**
    * Getter.
    *
    * @return identifier symtab entry, or null if the type is unnamed.
    */
  override def getIdentifier: SymTabEntry = identifier

  /**
    * Setter.
    *
    * @param identifier symtab entry for the identifier.
    */
  override def setIdentifier(identifier: SymTabEntry): Unit = this.identifier = identifier

  /**
    * Does this type represent a pascal string?
    *
    * @return true or false.
    */
  override def isPascalString: Boolean = {
    if (form == TypeFormImpl.ARRAY) {
      val elmtType = getAttribute(TypeKeyImpl.ARRAY_ELEMENT_TYPE).asInstanceOf[TypeSpec]
      val indexType = getAttribute(TypeKeyImpl.ARRAY_INDEX_TYPE).asInstanceOf[TypeSpec]

      elmtType.baseType == Predefined.charType &&
        indexType.baseType == Predefined.integerType
    } else {
      false
    }
  }

  /**
    * Getter, gets value from key.
    *
    * @param key attr key.
    * @return
    */
  override def getAttribute(key: TypeKey): Any = this.get(key)

  /**
    * Getter for base type.
    *
    * @return the base type of a subrange type, for other types it simply returns the type itself.
    */
  override def baseType: TypeSpec = {
    if (form.asInstanceOf[TypeFormImpl] == TypeFormImpl.SUBRANGE) {
      getAttribute(TypeKeyImpl.SUBRANGE_BASE_TYPE).asInstanceOf[TypeSpec]
    } else {
      this
    }
  }
}
