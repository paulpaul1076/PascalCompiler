package intermediate.icodeimpl

import intermediate.ICodeNodeType

/**
 * Class defining node types.
 */
class ICodeNodeTypeImpl(val name: String) extends ICodeNodeType {
  override def toString : String = name
}

/**
 * Constants for all types of nodes.
 */
object ICodeNodeTypeImpl {
  // Program structure
  val PROGRAM = new ICodeNodeTypeImpl("PROGRAM")
  val PROCEDURE = new ICodeNodeTypeImpl("PROCEDURE")
  val FUNCTION = new ICodeNodeTypeImpl("FUNCTION")

  // Statements
  val COMPOUND = new ICodeNodeTypeImpl("COMPOUND")
  val ASSIGN = new ICodeNodeTypeImpl("ASSIGN")
  val LOOP = new ICodeNodeTypeImpl("LOOP")
  val TEST = new ICodeNodeTypeImpl("TEST")
  val CALL = new ICodeNodeTypeImpl("CALL")
  val PARAMETERS = new ICodeNodeTypeImpl("PARAMETERS")
  val IF = new ICodeNodeTypeImpl("IF")
  val SELECT = new ICodeNodeTypeImpl("SELECT")
  val SELECT_BRANCH = new ICodeNodeTypeImpl("SELECT_BRANCH")
  val SELECT_CONSTANTS = new ICodeNodeTypeImpl("SELECT_CONSTANTS")
  val NO_OP = new ICodeNodeTypeImpl("NO_OP")

  // Relational operators
  val EQ = new ICodeNodeTypeImpl("EQ")
  val NE = new ICodeNodeTypeImpl("NE")
  val LT = new ICodeNodeTypeImpl("LT")
  val GT = new ICodeNodeTypeImpl("GT")
  val LE = new ICodeNodeTypeImpl("LE")
  val GE = new ICodeNodeTypeImpl("GE")
  val NOT = new ICodeNodeTypeImpl("NOT")

  // Additive operators.
  val ADD = new ICodeNodeTypeImpl("ADD")
  val SUBTRACT = new ICodeNodeTypeImpl("SUBTRACT")
  val OR = new ICodeNodeTypeImpl("OR")
  val NEGATE = new ICodeNodeTypeImpl("NEGATE")

  // Multiplicative operators
  val MULTIPLY = new ICodeNodeTypeImpl("MULTIPLY")
  val INTEGER_DIVIDE = new ICodeNodeTypeImpl("INTEGER_DIVIDE")
  val FLOAT_DIVIDE = new ICodeNodeTypeImpl("FLOAT_DIVIDE")
  val MOD = new ICodeNodeTypeImpl("MOD")
  val AND = new ICodeNodeTypeImpl("AND")

  // Operands
  val VARIABLE = new ICodeNodeTypeImpl("VARIABLE")
  val SUBSCRIPTS = new ICodeNodeTypeImpl("SUBSCRIPTS")
  val FIELD = new ICodeNodeTypeImpl("FIELD")
  val INTEGER_CONSTANT = new ICodeNodeTypeImpl("INTEGER_CONSTANT")
  val REAL_CONSTANT = new ICodeNodeTypeImpl("REAL_CONSTANT")
  val STRING_CONSTANT = new ICodeNodeTypeImpl("STRING_CONSTANT")
  val BOOLEAN_CONSTANT = new ICodeNodeTypeImpl("BOOLEAN_CONSTANT")

  // Write parameter
  val WRITE_PARM = new ICodeNodeTypeImpl("WRITE_PARM")
}