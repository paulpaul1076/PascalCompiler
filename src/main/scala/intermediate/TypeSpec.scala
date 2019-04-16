package intermediate

/**
  * The interface for a type specification.
  */
trait TypeSpec {
  /**
    * Getter.
    *
    * @return the type form (such as scalar, array, or record).
    */
  def getForm: TypeForm

  /**
    * Setter.
    *
    * @param identifier symtab entry for the identifier.
    */
  def setIdentifier(identifier: SymTabEntry): Unit

  /**
    * Getter.
    *
    * @return identifier symtab entry, or null if the type is unnamed.
    */
  def getIdentifier: SymTabEntry

  /**
    * Type attributes.
    *
    * @param key   attr key.
    * @param value attr value.
    */
  def setAttribute(key: TypeKey, value: Any): Unit

  /**
    * Getter, gets value from key.
    *
    * @param key attr key.
    * @return
    */
  def getAttribute(key: TypeKey): Any

  /**
    * Does this type represent a pascal string?
    *
    * @return true or false.
    */
  def isPascalString: Boolean

  /**
    * Getter for base type.
    *
    * @return the base type of a subrange type, for other types it simply returns the type itself.
    */
  def baseType: TypeSpec
}
