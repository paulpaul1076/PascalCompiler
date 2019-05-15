package intermediate.symtabimpl

import java.util

import intermediate.typeimpl.{TypeFormImpl, TypeKeyImpl}
import intermediate._

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

  // Standard routines.
  var readId: SymTabEntry = _
  var readlnId: SymTabEntry = _
  var writeId: SymTabEntry = _
  var writelnId: SymTabEntry = _
  var absId: SymTabEntry = _
  var arctanId: SymTabEntry = _
  var chrId: SymTabEntry = _
  var cosId: SymTabEntry = _
  var eofId: SymTabEntry = _
  var eolnId: SymTabEntry = _
  var expId: SymTabEntry = _
  var lnId: SymTabEntry = _
  var oddId: SymTabEntry = _
  var ordId: SymTabEntry = _
  var predId: SymTabEntry = _
  var roundId: SymTabEntry = _
  var sinId: SymTabEntry = _
  var sqrId: SymTabEntry = _
  var sqrtId: SymTabEntry = _
  var succId: SymTabEntry = _
  var truncId: SymTabEntry = _

  /**
    * Initialize a symbol table stack with predefined identifiers.
    *
    * @param symTabStack sym tab stack to initialize.
    */
  def initialize(symTabStack: SymTabStack): Unit = {
    initializeTypes(symTabStack)
    initializeConstants(symTabStack)
    initializeStandardRoutines(symTabStack)
  }

  /**
    * Initialize the standard procedures and functions.
    *
    * @param symTabStack the symbol table stack to initialize.
    */
  private def initializeStandardRoutines(symTabStack: SymTabStack): Unit = {
    readId = enterStandard(symTabStack, DefinitionImpl.PROCEDURE, "read", RoutineCodeImpl.READ)
    readlnId = enterStandard(symTabStack, DefinitionImpl.PROCEDURE, "readln", RoutineCodeImpl.READLN)
    writeId = enterStandard(symTabStack, DefinitionImpl.PROCEDURE, "write", RoutineCodeImpl.WRITE)
    writelnId = enterStandard(symTabStack, DefinitionImpl.PROCEDURE, "writeln", RoutineCodeImpl.WRITELN)

    absId = enterStandard(symTabStack, DefinitionImpl.FUNCTION, "abs", RoutineCodeImpl.ABS)
    arctanId = enterStandard(symTabStack, DefinitionImpl.FUNCTION, "arctan", RoutineCodeImpl.ARCTAN)
    chrId = enterStandard(symTabStack, DefinitionImpl.FUNCTION, "chr", RoutineCodeImpl.CHR)
    cosId = enterStandard(symTabStack, DefinitionImpl.FUNCTION, "cos", RoutineCodeImpl.COS)
    eofId = enterStandard(symTabStack, DefinitionImpl.FUNCTION, "eof", RoutineCodeImpl.EOF)
    eolnId = enterStandard(symTabStack, DefinitionImpl.FUNCTION, "eoln", RoutineCodeImpl.EOLN)
    expId = enterStandard(symTabStack, DefinitionImpl.FUNCTION, "exp", RoutineCodeImpl.EXP)
    lnId = enterStandard(symTabStack, DefinitionImpl.FUNCTION, "ln", RoutineCodeImpl.LN)
    oddId = enterStandard(symTabStack, DefinitionImpl.FUNCTION, "odd", RoutineCodeImpl.ODD)
    ordId = enterStandard(symTabStack, DefinitionImpl.FUNCTION, "ord", RoutineCodeImpl.ORD)
    predId = enterStandard(symTabStack, DefinitionImpl.FUNCTION, "pred", RoutineCodeImpl.PRED)
    roundId = enterStandard(symTabStack, DefinitionImpl.FUNCTION, "round", RoutineCodeImpl.ROUND)
    sinId = enterStandard(symTabStack, DefinitionImpl.FUNCTION, "sin", RoutineCodeImpl.SIN)
    sqrId = enterStandard(symTabStack, DefinitionImpl.FUNCTION, "sqr", RoutineCodeImpl.SQR)
    sqrtId = enterStandard(symTabStack, DefinitionImpl.FUNCTION, "sqrt", RoutineCodeImpl.SQRT)
    succId = enterStandard(symTabStack, DefinitionImpl.FUNCTION, "succ", RoutineCodeImpl.SUCC)
    truncId = enterStandard(symTabStack, DefinitionImpl.FUNCTION, "trunc", RoutineCodeImpl.TRUNC)
  }

  /**
    * Enter a standard procedure or function into the symbol table stack.
    *
    * @param symTabStack the symbol table stack to initialize.
    * @param defn        either PROCEDURE or FUNCTION.
    * @param name        the procedure or function name.
    * @param routineCode routine code (idk what that is) // TODO: find out
    * @return symbol table entry to enter into the level 0.
    */
  private def enterStandard(symTabStack: SymTabStack,
                            defn: Definition,
                            name: String,
                            routineCode: RoutineCode): SymTabEntry = {
    val procId = symTabStack.enterLocal(name)
    procId.setDefinition(defn)
    procId.setAttribute(SymTabKeyImpl.ROUTINE_CODE, routineCode)

    procId
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
