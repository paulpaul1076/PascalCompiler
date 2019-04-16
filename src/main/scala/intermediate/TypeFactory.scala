package intermediate

import intermediate.typeimpl.TypeSpecImpl

/**
  * A factory for creating type specifications.
  */
object TypeFactory {

  /**
    * Create a type specification of a given form.
    *
    * @param form form of the type.
    * @return the type specification.
    */
  def createType(form: TypeForm): TypeSpec = {
    new TypeSpecImpl(form)
  }

  /**
    * Create a type specification of a string.
    *
    * @param value the string value.
    * @return the type specification.
    */
  def createStringType(value: String): TypeSpec = {
    new TypeSpecImpl(value)
  }
}
