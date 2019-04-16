package intermediate.symtabimpl

import java.util

import intermediate.typeimpl.{TypeFormImpl, TypeKeyImpl}
import intermediate.{SymTabEntry, SymTabStack, TypeFactory, TypeSpec}

object Predefined {
  // Predefined types.
  var integerType: TypeSpec = _
  var realType: TypeSpec = _
  var booleanType: TypeSpec = _
  var charType: TypeSpec = _
  var undefinedType: TypeSpec = _

  // Predefined identifiers.
  var integerId: SymTabEntry = _
  var realId: SymTabEntry = _
  var booleanId: SymTabEntry = _
  var charId: SymTabEntry = _
  var falseId: SymTabEntry = _
  var trueId: SymTabEntry = _

  /**
    * Initialize a symbol table stack with predefined identifiers.
    *
    * @param symTabStack sym tab stack to initialize.
    */
  def initialize(symTabStack: SymTabStack): Unit = {
    initializeTypes(symTabStack)
    initializeConstants(symTabStack)
  }

  /**
    * Initialize the predefined types.
    *
    * @param symTabStack the sym tab stack to initialize.
    */
  def initializeTypes(symTabStack: SymTabStack): Unit = {
    // Type integer.
    integerId = symTabStack.enterLocal("integer")
    integerType = TypeFactory.createType(TypeFormImpl.SCALAR)
    integerType.setIdentifier(integerId)
    integerId.setDefinition(DefinitionImpl.TYPE)
    integerId.setTypeSpec(integerType)

    // Type real.
    realId = symTabStack.enterLocal("real")
    realType = TypeFactory.createType(TypeFormImpl.SCALAR)
    realType.setIdentifier(realId)
    realId.setDefinition(DefinitionImpl.TYPE)
    realId.setTypeSpec(realType)

    // Type boolean.
    booleanId = symTabStack.enterLocal("boolean")
    booleanType = TypeFactory.createType(TypeFormImpl.ENUMERATION)
    booleanType.setIdentifier(booleanId)
    booleanId.setDefinition(DefinitionImpl.TYPE)
    booleanId.setTypeSpec(booleanType)

    // Type char.
    charId = symTabStack.enterLocal("char")
    charType = TypeFactory.createType(TypeFormImpl.SCALAR)
    charType.setIdentifier(charId)
    charId.setDefinition(DefinitionImpl.TYPE)
    charId.setTypeSpec(charType)

    // Undefined type.
    undefinedType = TypeFactory.createType(TypeFormImpl.SCALAR)
  }

  /**
    * Initialize the predefined constant.
    *
    * @param symTabStack sym tab stack.
    */
  def initializeConstants(symTabStack: SymTabStack): Unit = {
    // Boolean enumeration constant false.
    falseId = symTabStack.enterLocal("false")
    falseId.setDefinition(DefinitionImpl.ENUMERATION_CONSTANT)
    falseId.setTypeSpec(booleanType)
    falseId.setAttribute(SymTabKeyImpl.CONSTANT_VALUE, 0)

    // Boolean enumeration constant true.
    trueId = symTabStack.enterLocal("true")
    trueId.setDefinition(DefinitionImpl.ENUMERATION_CONSTANT)
    trueId.setTypeSpec(booleanType)
    trueId.setAttribute(SymTabKeyImpl.CONSTANT_VALUE, 1)

    // Add false and true to the boolean enumeration type.
    val constants = new util.ArrayList[SymTabEntry]
    constants.add(falseId)
    constants.add(trueId)
    booleanType.setAttribute(TypeKeyImpl.ENUMERATION_CONSTANTS, constants)
  }
}
