package intermediate.typeimpl

import intermediate.TypeSpec
import intermediate.symtabimpl.Predefined

/**
  * This utility class implements Pascal's type compatibility rules.
  */
object TypeChecker {
  def areBothInteger(type1: TypeSpec, type2: TypeSpec): Boolean = {
    isInteger(type1) && isInteger(type2)
  }

  def isIntegerOrReal(`type`: TypeSpec): Boolean = {
    isInteger(`type`) || isReal(`type`)
  }

  def areBothBoolean(type1: TypeSpec, type2: TypeSpec): Boolean = {
    isBoolean(type1) && isBoolean(type2)
  }

  def isBoolean(`type`: TypeSpec): Boolean = {
    `type` != null && `type`.baseType == Predefined.booleanType
  }

  def isChar(`type`: TypeSpec): Boolean = {
    `type` != null && `type`.baseType == Predefined.charType
  }

  // Type compatibility methods
  def areAssignmentCompatible(targetType: TypeSpec, valueType: TypeSpec): Boolean = {
    if (targetType == null || valueType == null) {
      return false
    }

    val targetType1 = targetType.baseType
    val valueType1 = valueType.baseType

    var compatible = false

    // Identical types.
    if (targetType1 == valueType1) {
      compatible = true
    } // real := integer
    else if (isReal(targetType1) && isInteger(valueType1)) {
      compatible = true
    } // string := string
    else {
      compatible = targetType1.isPascalString && valueType1.isPascalString
    }
    return compatible
  }

  def isInteger(`type`: TypeSpec): Boolean = {
    `type` != null && `type`.baseType == Predefined.integerType
  }

  def isReal(`type`: TypeSpec): Boolean = {
    `type` != null && `type`.baseType == Predefined.realType
  }

  def areComparisonCompatible(type1: TypeSpec, type2: TypeSpec): Boolean = {
    if (type1 == null || type2 == null) {
      return false
    }

    val type1_ = type1.baseType
    val type2_ = type2.baseType

    val form = type1_.getForm

    var compatible = false

    // Two identical scalar or enumeration types.
    if (type1_ == type2_ && (form == TypeFormImpl.SCALAR || form == TypeFormImpl.ENUMERATION)) {
      compatible = true
    } // One integer and one real
    else if (isAtLeastOneReal(type1_, type2_)) {
      compatible = true
    } // Two strings.
    else {
      compatible = type1_.isPascalString && type2_.isPascalString
    }
    return compatible
  }

  def isAtLeastOneReal(type1: TypeSpec, type2: TypeSpec): Boolean = {
    (isReal(type1) && isReal(type2)) ||
      (isReal(type1) && isInteger(type2)) ||
      (isInteger(type1) && isReal(type2))
  }
}
